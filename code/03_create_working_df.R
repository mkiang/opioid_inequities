## Create a working df
##
## We want a dataframe of every state's age-standardized opioid mortality rate
## from 1999 to 2018 by race/ethnicity.
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

## Get all opioid deaths from 1999 to 2018 ----
opioid_file <- sprintf("opioid_deaths_%s_%i.RDS", year_0, year_n)
death_df <- readRDS(here::here(f_priv,  opioid_file))

death_summarized <- death_df %>%
    dplyr::mutate(
        race_eth = dplyr::case_when(
            hispanic_cat == "nonhispanic_white" ~ "nhw",
            hispanic_cat == "nonhispanic_black" ~ "nhb",
            TRUE ~ "other"
        )
    ) %>%
    dplyr::group_by(year, st_fips, age, race_eth) %>%
    dplyr::summarize(
        opioids = sum(opioid_death, na.rm = TRUE),
        heroin_present = sum(heroin_present, na.rm = TRUE),
        natural_present = sum(other_natural_present, na.rm = TRUE),
        synth_present = sum(other_synth_present, na.rm = TRUE)
    ) %>%
    dplyr::filter(race_eth != "other")

## Get all population counts from 1999 to 2018 ----
pop_df_full <- readr::read_csv(
    here::here(f_data, "pop_est_collapsed_long.csv.xz")
    )

pop_df <- pop_df_full %>% 
    dplyr::filter(year < 2019) %>%
    dplyr::mutate(
        race_eth = dplyr::case_when(
            hispanic == 0 & race == "black" ~ "nhb",
            hispanic == 0 & race == "white" ~ "nhw",
            TRUE ~ "other"
        )
    ) %>%
    dplyr::mutate(st_fips = substr(fipsihme, 1, 2)) %>%
    dplyr::group_by(st_fips, age, year, race_eth) %>%
    dplyr::summarize(pop_est = sum(pop_est)) %>%
    dplyr::ungroup() %>%
    dplyr::left_join(return_st_info()) %>%
    dplyr::filter(race_eth != "other")

## Get list of year/state observations to drop ----
## We want to eventually drop all observerations (year/state) with fewer
## than 100,000 nhw and nhb pop.
drop_df <- pop_df %>%
    dplyr::group_by(st_fips, year, race_eth, abbrev) %>%
    dplyr::summarize(pop = sum(pop_est)) %>%
    dplyr::ungroup() %>%
    tidyr::spread(race_eth, pop) %>%
    dplyr::mutate(drop_obs = !(nhb >= 100000 & nhw >= 100000)) %>%
    dplyr::select(year, abbrev, drop_obs)

pop_df <- dplyr::left_join(pop_df, drop_df)

## Join the pop counts with death counts, summarized by year/state/race ----
working_df <- pop_df %>%
    dplyr::left_join(death_summarized,
                     by = c("year", "st_fips", "age", "race_eth")) %>%
    dplyr::mutate_at(
        dplyr::vars(opioids, heroin_present, natural_present, synth_present),
        (function(x)
            ifelse(is.na(x), 0, x))
    ) %>%
    dplyr::rename(pop = pop_est) %>%
    narcan::add_std_pop(.) %>%
    dplyr::arrange(race_eth, abbrev, year, age)

## If a year/state has fewer than 100,000 black and white drop ----
working_df <- working_df %>%
    dplyr::filter(!drop_obs) %>%
    dplyr::select(-drop_obs)

## Impute small cells ----
## If a year/state/race group has > 100,000 people but 0 deaths, I impute it
## as 1 death spread across all age groups.
working_df <-
    working_df %>%
    dplyr::group_by(year, st_fips, race_eth, abbrev) %>%
    dplyr::mutate(
        pop100k = sum(pop) >= 100000,
        no_opioids = sum(opioids) == 0,
        no_heroin = sum(heroin_present) == 0,
        no_natural = sum(natural_present) == 0,
        no_synth = sum(synth_present) == 0
    ) %>%
    dplyr::mutate(
        opioids = ifelse(pop100k & no_opioids, 1 / 18, opioids),
        heroin_present = ifelse(pop100k &
                                    no_heroin, 1 / 18, heroin_present),
        natural_present = ifelse(pop100k &
                                     no_natural, 1 / 18, natural_present),
        synth_present = ifelse(pop100k &
                                   no_synth, 1 / 18, synth_present)
    ) %>%
    dplyr::ungroup() %>%
    dplyr::select(-pop100k, -no_opioids, -no_heroin, -no_natural, -no_synth)

## Calculate the age-specific rates for each opioid type ----
working_df <- working_df %>%
    narcan::calc_asrate_var(., opioid, opioids) %>%
    dplyr::left_join(
        working_df %>%
            narcan::calc_asrate_var(., heroin, heroin_present) %>%
            dplyr::select(st_fips,
                          age,
                          year,
                          race_eth,
                          dplyr::starts_with("heroin"))
    ) %>%
    dplyr::left_join(
        working_df %>%
            narcan::calc_asrate_var(., natural, natural_present) %>%
            dplyr::select(st_fips,
                          age,
                          year,
                          race_eth,
                          dplyr::starts_with("natural"))
    ) %>%
    dplyr::left_join(
        working_df %>%
            narcan::calc_asrate_var(., synth, synth_present) %>%
            dplyr::select(st_fips,
                          age,
                          year,
                          race_eth,
                          dplyr::starts_with("synth"))
    )

## Calculate the age-standardized rates for each opioid type ----
age_std_rates_wide <- working_df %>%
    narcan::calc_stdrate_var(opioid_rate,
                             opioid_var,
                             year,
                             st_fips,
                             race_eth,
                             abbrev,
                             name) %>%
    dplyr::left_join(
        working_df %>%
            narcan::calc_stdrate_var(heroin_rate,
                                     heroin_var,
                                     year,
                                     st_fips,
                                     race_eth,
                                     abbrev,
                                     name)
    ) %>%
    dplyr::left_join(
        working_df %>%
            narcan::calc_stdrate_var(natural_rate,
                                     natural_var,
                                     year,
                                     st_fips,
                                     race_eth,
                                     abbrev,
                                     name)
    ) %>%
    dplyr::left_join(
        working_df %>%
            narcan::calc_stdrate_var(synth_rate,
                                     synth_var,
                                     year,
                                     st_fips,
                                     race_eth,
                                     abbrev,
                                     name)
    )

## Reshape it to long ----
age_std_rates_long <- age_std_rates_wide %>%
    dplyr::select(year,
                  st_fips,
                  race_eth,
                  abbrev,
                  name,
                  dplyr::ends_with("_rate")) %>%
    tidyr::gather(opioid_type, rate, opioid_rate:synth_rate) %>%
    dplyr::mutate(opioid_type = gsub("_rate", "", opioid_type)) %>%
    dplyr::left_join(
        age_std_rates_wide %>%
            dplyr::select(year, st_fips, race_eth, abbrev, name, dplyr::ends_with("_var")) %>%
            tidyr::gather(opioid_type, var, opioid_var:synth_var) %>%
            dplyr::mutate(opioid_type = gsub("_var", "", opioid_type))
    ) %>%
    dplyr::ungroup()

## Add back in the death count and population ----
age_std_rates_long <- age_std_rates_long %>%
    dplyr::left_join(
        death_summarized %>%
            dplyr::group_by(year, st_fips, race_eth) %>%
            dplyr::summarize_at(dplyr::vars(opioids:synth_present), sum) %>%
            tidyr::gather(opioid_type, deaths, opioids:synth_present) %>%
            dplyr::mutate(opioid_type = gsub("_present", "", opioid_type)) %>%
            dplyr::mutate(opioid_type = gsub("opioids", "opioid", opioid_type))
    ) %>%
    dplyr::left_join(pop_df %>%
                         dplyr::group_by(st_fips, year, race_eth) %>%
                         dplyr::summarize(pop = sum(pop_est, na.rm = TRUE))) %>%
    dplyr::mutate(deaths = ifelse(is.na(deaths), 0, deaths),
                  sd = sqrt(var))

## Remove all cohorts with less than 75% of the years of observation ----
age_std_rates_long <- age_std_rates_long %>%
    dplyr::group_by(st_fips, race_eth, opioid_type) %>%
    dplyr::filter(dplyr::n() >= round(.75 * dplyr::n_distinct(working_df$year))) %>%
    dplyr::arrange(race_eth, abbrev, opioid_type, year) %>%
    dplyr::ungroup()

## Save out to joinpoint file
readr::write_csv(age_std_rates_long,
                 here::here("joinpoint_analyses", "age_std_rates_long.csv"))
