## Retrieve all opioid-related deaths ----
## This script goes through all the private files (using the regex pattern
## on line 33) and extracts all opioid-related deaths, saving just the
## subset of deaths we need for our analysis.

## Imports ----
library(tidyverse)
library(narcan)   # install_github("mkiang/narcan")
library(fs)
library(here)
library(foreach)
library(doParallel)
library(config)
source(here::here("code", "utils.R"))

## Constants ----
multi_proc <- config::get("proc_in_parallel")
n_cores <- ifelse(multi_proc, config::get("num_cores"), 1)
year_0 <- config::get("year_0")
year_n <- config::get("year_n")
f_temp <- config::get("temp_files")
f_priv <- config::get("private_data")
keep_temps <- config::get("keep_working_data")

## Subset the private files ----
## All the private files are stored in `raw_data` and follow the format:
##  MULTYYYY.USPSAllCnty.zip where YYYY is the year. See README.md.
doParallel::registerDoParallel(cores = n_cores)
foreach::foreach(year = year_0:year_n, .inorder = FALSE) %dopar% {
    f_old <- fs::dir_ls(here::here(f_priv),
                        regexp = sprintf("MULT%i\\.[[:alpha:]]+\\.zip", year))
    f_new <- here::here(f_temp, sprintf("opioid_deaths_%s.RDS", year))
    
    if (!fs::file_exists(f_new)) {
        temp_df <- narcan:::.import_restricted_data(f_old, 
                                                    year,
                                                    fix_states = FALSE)
        
        ## US residents only
        temp_df <- temp_df %>%
            narcan::subset_residents(.)
        
        ## Subset to columns we will need
        temp_df <- temp_df %>%
            dplyr::select(
                dplyr::one_of(
                    c(
                        "year",
                        "monthdth",
                        "countyoc",
                        "countyrs",
                        "ager27",
                        "ucod",
                        "race",
                        "hspanicr"
                    )
                ),
                dplyr::starts_with("record_"),
                dplyr::starts_with("rnifla")
            ) %>%
            narcan::add_hspanicr_column(.)
        
        ## Unite all 20 contributory cause columns
        temp_df <- narcan::unite_records(temp_df)
        
        ## Convert age and extract state
        temp_df <- temp_df %>%
            narcan::convert_ager27(.) %>%
            narcan::remap_race(.) %>%
            dplyr::mutate(
                race_cat = narcan::categorize_race(race),
                hispanic_cat = narcan::categorize_hspanicr(hspanicr),
                age_cat  = narcan::categorize_age_5(age),
                county_substr = substr(countyoc, 1, 2)
            )
        
        ## Figure out the correct state coding ----
        substr_codes <- unique(temp_df$county_substr)
        if (all(substr_codes %in% c("DC", datasets::state.abb))) {
            ## Abbreviations?
            temp_df <- temp_df %>%
                dplyr::mutate(st_fips = narcan::state_abbrev_to_fips(county_substr))
        } else if (all(substr_codes %in% unique(st_info$nchs_fips))) {
            ## NCHS state codes
            temp_df <- temp_df %>%
                dplyr::left_join(st_info %>%
                                     dplyr::select(st_fips, county_substr = nchs_fips))
        } else if (all(substr_codes %in% unique(st_info$st_fips))) {
            temp_df <- temp_df %>%
                dplyr::mutate(st_fips = substr_codes)
        }
        temp_df <- temp_df %>%
            dplyr::select(-county_substr)
        
        ## Reorder columns
        temp_df <- temp_df %>%
            dplyr::select(
                year,
                st_fips,
                age,
                age_cat,
                race,
                race_cat,
                hispanic_cat,
                ucod,
                f_records_all,
                dplyr::everything()
            )
        
        ## Find opioid deaths
        temp_df <- temp_df %>%
            narcan::flag_opioid_deaths(., year = year) %>%
            narcan::flag_opioid_types(., year = year)
        
        ## Subset to just opioid deaths
        temp_df <- temp_df %>%
            dplyr::filter(opioid_death == 1)
        
        ## Join with other state info
        temp_df <- temp_df %>%
            dplyr::left_join(return_st_info())
        
        saveRDS(temp_df, f_new)
    }
}
doParallel::stopImplicitCluster()

## Collect all files ----
opioid_file <- sprintf("opioid_deaths_%s_%i.RDS", year_0, year_n)
if (!fs::file_exists(here::here(f_priv, opioid_file))) {
    all_opioid_deaths <- purrr::map_df(
        .x = fs::dir_ls(here::here(f_temp),
                        type = "file",
                        regexp = "opioid_deaths_[0-9]{4}\\.RDS"),
        .f = ~ readRDS(.x)
    )
    saveRDS(all_opioid_deaths,
            here::here(f_priv, opioid_file))
    
    ## Clean up temp files ----
    if (!keep_temps) {
        fs::file_delete(
            fs::dir_ls(
                here::here(f_temp),
                type = "file",
                regexp = "opioid_deaths_[0-9]{4}\\.RDS"
            )
        )
    }
}
