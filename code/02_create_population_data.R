## 02_create_population_data.R ----
## Tasks in this file:
##      (1) If population files do not exist yet, download them.
##      (2) Reshape and convert to 5-year age groups then save.

## Imports ----
library(tidyverse)
library(here)
library(fs)
library(config)
library(narcan)

## Constants ----
data_folder <- config::get("working_data")
keep_temps <- config::get("keep_working_data")

## Make a dictionary that goes from FIPS to IHME stable fips ----
## Counties change over time. There are different sets of "temporally-stable"
## county mappings but the most common is the IHME one, which is included in
## the narcan package.
fips_pattern <- narcan::ihme_fips$ihme_fips
names(fips_pattern) <- narcan::ihme_fips$orig_fips

## (1) Downloading files ----
## 1999 data
if (!fs::file_exists(here::here(data_folder, "icen1999.txt.zip"))) {
    utils::download.file(
        url = paste0(
            "ftp://ftp.cdc.gov/pub/Health_Statistics/NCHS/",
            "datasets/nvss/bridgepop/icen1999.txt"
        ),
        destfile = here::here(data_folder, "icen1999.txt")
    )
    
    utils::zip(
        files = here::here(data_folder, "icen1999.txt"),
        zipfile = here::here(data_folder, "icen1999.txt.zip")
    )
    
    file.remove(here::here(data_folder, "icen1999.txt"))
}

## 2000-2004 data
if (!fs::file_exists(here::here(data_folder, "icen_2000_09_y0004.zip"))) {
    utils::download.file(
        url = paste0(
            "ftp://ftp.cdc.gov/pub/Health_Statistics/NCHS/",
            "datasets/nvss/bridgepop/2000_09/",
            "icen_2000_09_y0004.zip"
        ),
        destfile = here::here(data_folder, "icen_2000_09_y0004.zip")
    )
}

## 2005-2009 data
if (!fs::file_exists(here::here(data_folder, "icen_2000_09_y0509.zip"))) {
    utils::download.file(
        url = paste0(
            "ftp://ftp.cdc.gov/pub/Health_Statistics/NCHS/",
            "datasets/nvss/bridgepop/2000_09/",
            "icen_2000_09_y0509.zip"
        ),
        destfile = here::here(data_folder, "icen_2000_09_y0509.zip")
    )
}

## 2010-2019 data (get the 2019 Vintage)
if (!fs::file_exists(here::here(data_folder, "pcen_v2019_y1019_txt.zip"))) {
    utils::download.file(
        url = paste0(
            "https://www.cdc.gov/nchs/nvss/", 
            "bridged_race/pcen_v2019_y1019_txt.zip"
        ),
        destfile = here::here(data_folder, "pcen_v2019_y1019_txt.zip")
    )
}


## (2) Reshape population data ----
## Import population files in wide format where columns are: ----
##      fipst, fipsct, age, racesex, hispanic, pop_est_XXXX
## with XXXX being a year between 1999-2015 and age representing 5-year age
## in 0-4, 5-9, ... , 85+ groupings.
pop1999 <-
    readr::read_fwf(here::here(data_folder, "icen1999.txt.zip"),
                    readr::fwf_widths(
                        c(4, 2, 3, 2, 1, 1, 8),
                        c(
                            "year",
                            "fipst",
                            "fipsct",
                            "age",
                            "racesex",
                            "hispanic",
                            "pop_est_1999"
                        )
                    )) %>%
    dplyr::select(-year) %>%
    ## Right now age is 0, 1-4, 5-9, etc. Collapse and make age sensible.
    dplyr::mutate(age = ifelse(age > 0, (age - 1) * 5, age)) %>%
    dplyr::group_by(fipst, fipsct, age, racesex, hispanic) %>%
    dplyr::summarize(pop_est_1999 = sum(pop_est_1999)) %>%
    dplyr::ungroup()

width_00s <- c(8, 2, 3, 2, 1, 1, 8, 8, 8, 8, 8)
pop2000s <-
    dplyr::left_join(
        ## We can left_join() here because 2000-2009 have the same FIPS
        readr::read_fwf(
            here::here(data_folder, "icen_2000_09_y0004.zip"),
            readr::fwf_widths(
                width_00s,
                c(
                    "series",
                    "fipst",
                    "fipsct",
                    "age",
                    "racesex",
                    "hispanic",
                    "pop_est_2000",
                    "pop_est_2001",
                    "pop_est_2002",
                    "pop_est_2003",
                    "pop_est_2004"
                )
            )
        ),
        readr::read_fwf(
            here::here(data_folder, "icen_2000_09_y0509.zip"),
            readr::fwf_widths(
                width_00s,
                c(
                    "series",
                    "fipst",
                    "fipsct",
                    "age",
                    "racesex",
                    "hispanic",
                    "pop_est_2005",
                    "pop_est_2006",
                    "pop_est_2007",
                    "pop_est_2008",
                    "pop_est_2009"
                )
            )
        )
    ) %>%
    dplyr::select(-series) %>%
    dplyr::mutate(age = (cut(
        age,
        c(0, seq(5, 85, 5), Inf),
        include.lowest = TRUE,
        right = FALSE,
        labels = FALSE
    ) - 1) * 5) %>%
    dplyr::group_by(fipst, fipsct, age, racesex, hispanic) %>%
    dplyr::summarize_all(sum) %>%
    dplyr::ungroup()

pop2010s <-
    readr::read_fwf(here::here(data_folder, "pcen_v2019_y1019_txt.zip"),
                    readr::fwf_widths(
                        c(4, 2, 3, 2, 1, 1, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8),
                        c(
                            "series",
                            "fipst",
                            "fipsct",
                            "age",
                            "racesex",
                            "hispanic",
                            "pop_est_2010april",
                            "pop_est_2010",
                            "pop_est_2011",
                            "pop_est_2012",
                            "pop_est_2013",
                            "pop_est_2014",
                            "pop_est_2015",
                            "pop_est_2016",
                            "pop_est_2017",
                            "pop_est_2018",
                            "pop_est_2019"
                        )
                    ))  %>%
    dplyr::select(-series) %>%
    dplyr::mutate(age = (cut(
        age,
        c(0, seq(5, 85, 5), Inf),
        include.lowest = TRUE,
        right = FALSE,
        labels = FALSE
    ) - 1) * 5) %>%
    dplyr::group_by(fipst, fipsct, age, racesex, hispanic) %>%
    dplyr::summarize_all(sum) %>%
    dplyr::ungroup()

## Now modify each population dataframe with better race/sex/hispanic columns
## such that race and sex are separated and hispanic/female are indicators.
## Also add the IHME collapsed FIPS as a separate column.
clean_pop_data <- function(df) {
    newdf <- df %>%
        dplyr::ungroup() %>%
        dplyr::mutate(
            race = dplyr::case_when(
                racesex %in% 1:2 ~ "white",
                racesex %in% 3:4 ~ "black",
                racesex %in% 5:6 ~ "aia",
                racesex %in% 7:8 ~ "api"
            ),
            female = ifelse(racesex %% 2 == 0, 1, 0),
            hispanic = hispanic - 1,
            fipschar = paste0(fipst, fipsct),
            fipsihme = stringr::str_replace_all(fipschar, fips_pattern)
        )
    return(newdf)
}

pop1999  <- clean_pop_data(pop1999)
pop2000s <- clean_pop_data(pop2000s)
pop2010s <- clean_pop_data(pop2010s)

## Now collapse over sex and the IHME FIPS codes ----
## Then combine into a single file.
collapse_df <- function(df) {
    newdf <- df %>%
        dplyr::select(fipsihme,
                      age,
                      race,
                      hispanic,
                      dplyr::starts_with("pop_est_")) %>%
        dplyr::group_by(fipsihme, age, race, hispanic) %>%
        dplyr::summarize_all(sum)
    return(newdf)
}

pop1999  <- collapse_df(pop1999)
pop2000s <- collapse_df(pop2000s)
pop2010s <- collapse_df(pop2010s)

all_pops_wide <- pop1999 %>%
    dplyr::left_join(pop2000s) %>%
    dplyr::left_join(pop2010s) %>%
    dplyr::ungroup()

## Convert to long
all_pops_long <- all_pops_wide %>%
    dplyr::ungroup() %>%
    dplyr::select(-dplyr::ends_with("april")) %>%
    dplyr::select(fipst,
                  fipsct,
                  age,
                  racesex,
                  hispanic,
                  race,
                  female,
                  fipschar,
                  fipsihme,
                  dplyr::everything()) %>%
    tidyr::gather(year, pop_est, pop_est_1999:pop_est_2019) %>%
    dplyr::mutate(year = as.integer(gsub("pop_est_", "", year)))

## Save ----
readr::write_csv(all_pops_long,
                 here::here(data_folder, "pop_est_collapsed_long.csv.xz"))

## Clean up ----
if (!keep_temps) {
    fs::file_delete(here::here(
        data_folder,
        c(
            "/icen1999.txt.zip",
            "/icen_2000_09_y0004.zip",
            "/icen_2000_09_y0509.zip",
            "/pcen_v2019_y1019_txt.zip"
        )
    ))
}
