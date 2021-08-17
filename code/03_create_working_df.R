## Create a working df
##
## We want a dataframe of every state's age-standardized opioid mortality rate
## from 1999 to 2019 by race/ethnicity.
library(tidyverse)
library(here)
library(narcan)
library(config)
source(here::here("code", "utils.R"))

## Constants ----
f_priv <- config::get("private_data")
f_data <- config::get("working_data")
year_0 <- config::get("year_0")
year_n <- config::get("year_n")

## Make a dictionary that goes from FIPS to IHME stable fips ----
## Counties change over time. There are different sets of "temporally-stable"
## county mappings but the most common is the IHME one, which is included in
## the narcan package.
fips_pattern <- narcan::ihme_fips$ihme_fips
names(fips_pattern) <- narcan::ihme_fips$orig_fips

## Death data ----
opioid_file <- sprintf("opioid_deaths_%s_%i.RDS", year_0, year_n)
death_df <- readRDS(here::here(f_priv,  opioid_file)) %>%
    mutate(fipsihme = stringr::str_replace_all(county_fips, fips_pattern)) %>%
    ungroup()

## Get all population counts from 1999 to 2019 ----
## Read in the population, convert to NHW/NHB/Other
pop_df_full <-
    readr::read_csv(here::here(f_data, "pop_est_collapsed_long.csv.xz")) %>%
    dplyr::mutate(
        race_eth = dplyr::case_when(
            hispanic == 0 & race == "black" ~ "nhb",
            hispanic == 0 & race == "white" ~ "nhw",
            TRUE ~ "other"
        )
    )

## We want states but we also want counties with a large NHB population ----
counties_to_save <- pop_df_full %>%
    group_by(fipsihme, year, race_eth) %>%
    summarize(pop_est = sum(pop_est)) %>%
    group_by(fipsihme, race_eth) %>%
    summarize(pop_est = mean(pop_est)) %>% 
    ungroup() %>% 
    pivot_wider(fipsihme, names_from = "race_eth", values_from = "pop_est") %>% 
    filter(nhw >= 100000 & nhb >= 100000) %>%
    pull(fipsihme) %>%
    unique()

## Add total pop and then collapse to NHW, NHB, Other, and Total pop ----
pop_df <- bind_rows(pop_df_full,
                    pop_df_full %>%
                        mutate(race_eth = "total"))

## Add in the counties we want to keep
pop_df <- bind_rows(
    pop_df %>%
        dplyr::mutate(st_fips = substr(fipsihme, 1, 2)),
    pop_df %>%
        filter(fipsihme %in% counties_to_save) %>%
        mutate(st_fips = fipsihme)
)

## Aggregate to states and counties of interest
pop_df <-  pop_df %>%
    dplyr::group_by(race_eth, st_fips, year, age) %>%
    dplyr::summarize(pop_est = sum(pop_est)) %>%
    dplyr::ungroup() %>%
    dplyr::left_join(return_st_info())

## Get all opioid deaths from 1999 to 2019 ----
death_combined <- bind_rows(
    death_df %>%
        dplyr::mutate(
            race_eth = dplyr::case_when(
                hispanic_cat == "nonhispanic_white" ~ "nhw",
                hispanic_cat == "nonhispanic_black" ~ "nhb",
                TRUE ~ "other"
            )
        ),
    death_df %>%
        mutate(race_eth = "total")
) %>%
    ungroup()

death_combined <- bind_rows(
    death_combined %>%
        dplyr::mutate(st_fips = substr(fipsihme, 1, 2)),
    death_combined %>%
        filter(fipsihme %in% counties_to_save) %>%
        mutate(st_fips = fipsihme)
)

death_summarized <- death_combined %>%
    dplyr::group_by(year, st_fips, age, race_eth) %>%
    dplyr::summarize(
        opioids = sum(opioid_death, na.rm = TRUE),
        heroin_present = sum(heroin_present, na.rm = TRUE),
        natural_present = sum(other_natural_present, na.rm = TRUE),
        synth_present = sum(other_synth_present, na.rm = TRUE)
    ) %>%
    ungroup() %>%
    left_join(
        death_combined %>%
            filter(num_opioids == 1) %>%
            dplyr::group_by(year, st_fips, age, race_eth) %>%
            dplyr::summarize(
                one_opioid = sum(opioid_death, na.rm = TRUE),
                heroin_alone = sum(heroin_present, na.rm = TRUE),
                natural_alone = sum(other_natural_present, na.rm = TRUE),
                synth_alone = sum(other_synth_present, na.rm = TRUE)
            ) %>%
            ungroup()
    ) %>%
    left_join(
        death_combined %>%
            filter(num_opioids > 1) %>%
            dplyr::group_by(year, st_fips, age, race_eth) %>%
            dplyr::summarize(multi_opioid = sum(opioid_death, na.rm = TRUE)) %>%
            ungroup()
    )

working_df <- pop_df %>%
    dplyr::left_join(death_summarized,
                     by = c("year", "st_fips", "age", "race_eth")) %>%
    dplyr::mutate_at(
        dplyr::vars(
            opioids,
            heroin_present,
            natural_present,
            synth_present,
            one_opioid,
            heroin_alone,
            natural_alone,
            synth_alone,
            multi_opioid
        ),
        (function(x)
            ifelse(is.na(x), 0, x))
    ) %>%
    dplyr::rename(pop = pop_est) %>%
    left_join(st_fips_map %>%
                  select(
                      st_name = name,
                      st_abbrev = abbrev,
                      st_fips = fips
                  )) %>%
    narcan::add_std_pop(.) %>%
    merge_new_abbrev() %>%
    dplyr::arrange(race_eth, st_fips, abbrev, year, age)

working_df <- working_df %>%
    ungroup() %>%
    group_by(race_eth, st_fips, year) %>%
    mutate(
        opioids = case_when(sum(opioids) == 0 ~ .1 / 18, TRUE ~ opioids),
        heroin_present = case_when(sum(heroin_present) == 0 ~ .1 / 18, TRUE ~ heroin_present),
        natural_present = case_when(sum(natural_present) == 0 ~ .1 / 18, TRUE ~ natural_present),
        synth_present = case_when(sum(synth_present) == 0 ~ .1 / 18, TRUE ~ synth_present),
        one_opioid = case_when(sum(one_opioid) == 0 ~ .1 / 18, TRUE ~ one_opioid),
        heroin_alone = case_when(sum(heroin_alone) == 0 ~ .1 / 18, TRUE ~ heroin_alone),
        natural_alone = case_when(sum(natural_alone) == 0 ~ .1 / 18, TRUE ~ natural_alone),
        synth_alone = case_when(sum(synth_alone) == 0 ~ .1 / 18, TRUE ~ synth_alone),
        multi_opioid = case_when(sum(multi_opioid) == 0 ~ .1 / 18, TRUE ~ multi_opioid)
    )

collapsed_deaths <- working_df %>%
    narcan::calc_asrate_var(., opioid, opioids) %>%
    select(
        st_fips,
        age,
        year,
        race_eth,
        pop,
        st_cat,
        abbrev,
        division,
        st_lat,
        st_lon,
        lon_rank,
        name,
        name_cat,
        name_cat_alpha,
        alpha_rank,
        pop_std,
        unit_w,
        opioids,
        opioid_rate,
        opioid_var
    ) %>%
    dplyr::left_join(
        working_df %>%
            narcan::calc_asrate_var(., heroin, heroin_present) %>%
            dplyr::select(st_fips,
                          age,
                          year,
                          race_eth,
                          dplyr::starts_with("heroin")) %>%
            select(-ends_with("_alone"))
    ) %>%
    dplyr::left_join(
        working_df %>%
            narcan::calc_asrate_var(., natural, natural_present) %>%
            dplyr::select(st_fips,
                          age,
                          year,
                          race_eth,
                          dplyr::starts_with("natural")) %>%
            select(-ends_with("_alone"))
    ) %>%
    dplyr::left_join(
        working_df %>%
            narcan::calc_asrate_var(., synth, synth_present) %>%
            dplyr::select(st_fips,
                          age,
                          year,
                          race_eth,
                          dplyr::starts_with("synth")) %>%
            select(-ends_with("_alone"))
    ) %>%
    dplyr::left_join(
        working_df %>%
            narcan::calc_asrate_var(., single_opioid, one_opioid) %>%
            dplyr::select(
                st_fips,
                age,
                year,
                race_eth,
                dplyr::starts_with("single_opioid")
            )
    ) %>%
    dplyr::left_join(
        working_df %>%
            narcan::calc_asrate_var(., single_heroin, heroin_alone) %>%
            dplyr::select(
                st_fips,
                age,
                year,
                race_eth,
                dplyr::starts_with("single_heroin")
            )
    ) %>%
    dplyr::left_join(
        working_df %>%
            narcan::calc_asrate_var(., single_natural, natural_alone) %>%
            dplyr::select(
                st_fips,
                age,
                year,
                race_eth,
                dplyr::starts_with("single_natural")
            )
    ) %>%
    dplyr::left_join(
        working_df %>%
            narcan::calc_asrate_var(., single_synth, synth_alone) %>%
            dplyr::select(
                st_fips,
                age,
                year,
                race_eth,
                dplyr::starts_with("single_synth")
            )
    )  %>%
    dplyr::left_join(
        working_df %>%
            narcan::calc_asrate_var(., poly_opioid, multi_opioid) %>%
            dplyr::select(
                st_fips,
                age,
                year,
                race_eth,
                dplyr::starts_with("poly_opioid")
            )
    )

## Remove groups with just no deaths
collapsed_deaths <- collapsed_deaths %>%
    group_by(race_eth, st_fips) %>%
    filter(!(sum(opioids < 1) == n_distinct(year))) %>%
    ungroup()

## Calculate the age-standardized rates for each opioid type ----
age_std_rates_wide <- collapsed_deaths %>%
    narcan::calc_stdrate_var(opioid_rate,
                             opioid_var,
                             year,
                             st_fips,
                             race_eth,
                             abbrev,
                             name) %>%
    dplyr::left_join(
        collapsed_deaths %>%
            narcan::calc_stdrate_var(heroin_rate,
                                     heroin_var,
                                     year,
                                     st_fips,
                                     race_eth,
                                     abbrev,
                                     name)
    ) %>%
    dplyr::left_join(
        collapsed_deaths %>%
            narcan::calc_stdrate_var(natural_rate,
                                     natural_var,
                                     year,
                                     st_fips,
                                     race_eth,
                                     abbrev,
                                     name)
    ) %>%
    dplyr::left_join(
        collapsed_deaths %>%
            narcan::calc_stdrate_var(synth_rate,
                                     synth_var,
                                     year,
                                     st_fips,
                                     race_eth,
                                     abbrev,
                                     name)
    ) %>%
    dplyr::left_join(
        collapsed_deaths %>%
            narcan::calc_stdrate_var(
                single_opioid_rate,
                single_opioid_var,
                year,
                st_fips,
                race_eth,
                abbrev,
                name
            )
    ) %>%
    dplyr::left_join(
        collapsed_deaths %>%
            narcan::calc_stdrate_var(
                single_heroin_rate,
                single_heroin_var,
                year,
                st_fips,
                race_eth,
                abbrev,
                name
            )
    ) %>%
    dplyr::left_join(
        collapsed_deaths %>%
            narcan::calc_stdrate_var(
                single_natural_rate,
                single_natural_var,
                year,
                st_fips,
                race_eth,
                abbrev,
                name
            )
    ) %>%
    dplyr::left_join(
        collapsed_deaths %>%
            narcan::calc_stdrate_var(
                single_synth_rate,
                single_synth_var,
                year,
                st_fips,
                race_eth,
                abbrev,
                name
            )
    ) %>%
    dplyr::left_join(
        collapsed_deaths %>%
            narcan::calc_stdrate_var(
                poly_opioid_rate,
                poly_opioid_var,
                year,
                st_fips,
                race_eth,
                abbrev,
                name
            )
    )

## Reshape it to long ----
age_std_rates_long <- age_std_rates_wide %>%
    dplyr::select(year,
                  st_fips,
                  race_eth,
                  abbrev,
                  name,
                  dplyr::ends_with("_rate")) %>%
    tidyr::gather(opioid_type, rate, opioid_rate:poly_opioid_rate) %>%
    dplyr::mutate(opioid_type = gsub("_rate", "", opioid_type)) %>%
    dplyr::left_join(
        age_std_rates_wide %>%
            dplyr::select(year, st_fips, race_eth, abbrev, name, dplyr::ends_with("_var")) %>%
            tidyr::gather(opioid_type, var, opioid_var:poly_opioid_var) %>%
            dplyr::mutate(opioid_type = gsub("_var", "", opioid_type))
    ) %>%
    dplyr::ungroup()

## Add back in the death count and population ----
age_std_rates_long <- age_std_rates_long %>%
    dplyr::left_join(
        death_summarized %>%
            dplyr::group_by(year, st_fips, race_eth) %>%
            dplyr::summarize_at(dplyr::vars(opioids:multi_opioid), sum, na.rm = TRUE) %>%
            tidyr::gather(opioid_type, deaths, opioids:multi_opioid) %>%
            mutate(
                opioid_type = case_when(
                    opioid_type == "opioids" ~ "opioid",
                    opioid_type == "heroin_present" ~ "heroin",
                    opioid_type == "natural_present" ~ "natural",
                    opioid_type == "synth_present" ~ "synth",
                    opioid_type == "one_opioid" ~ "single_opioid",
                    opioid_type == "heroin_alone" ~ "single_heroin",
                    opioid_type == "natural_alone" ~ "single_natural",
                    opioid_type == "synth_alone" ~ "single_synth",
                    opioid_type == "multi_opioid" ~ "poly_opioid"
                )
            )
    ) %>%
    dplyr::left_join(pop_df %>%
                         dplyr::group_by(st_fips, year, race_eth) %>%
                         dplyr::summarize(pop = sum(pop_est, na.rm = TRUE)))

age_std_rates_long <- age_std_rates_long %>%
    dplyr::mutate(deaths = ifelse(is.na(deaths), 0, deaths)) %>% 
    dplyr::mutate(var = ifelse(deaths == 0, 0.05, var)) %>% 
    dplyr::mutate(sd = sqrt(var))

## Remove all cohorts with less than 50% of the years of observation ----
age_std_rates_long <- age_std_rates_long %>%
    filter(race_eth != "other") %>%
    dplyr::group_by(st_fips, race_eth, opioid_type) %>%
    filter(sum(deaths > 0) >= 10) %>%
    dplyr::ungroup() %>%
    dplyr::arrange(race_eth, abbrev, opioid_type, year)

## Save out to joinpoint file
readr::write_csv(age_std_rates_long,
                 here::here("joinpoint_analyses", "age_std_rates_long.csv"))

## Save an R obj version with correct factors for joining
saveRDS(age_std_rates_long,
        here::here("data_private", "age_std_rates_long.RDS"))
