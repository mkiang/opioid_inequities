## Imports ----
library(tidyverse)
library(here)
source(here::here("code", "mk_nytimes.R"))
source(here::here("code", "utils.R"))

## We want a dataframe with the diffs and ratios and 95% cis, but
## have the correctly suppressed rates. We do that by running the
## functions through the unsuppressed data, dropping the rates columns,
## then running it again with the suppressed data and joining them.
## Data ----
wide_df_supp <-
    readr::read_csv(here::here("data", "joinpoint_estimates_suppressed.csv")) %>%
    categorize_opioids() %>%
    categorize_race() %>%
    reshape_to_yearly_wide(1999:2018) %>%
    calculate_ratios() %>%
    calculate_diffs()

wide_df_all <-
    readr::read_csv(here::here("data_private", "joinpoint_estimates_all.csv")) %>%
    categorize_opioids() %>%
    categorize_race() %>%
    reshape_to_yearly_wide(1999:2018) %>%
    calculate_ratios() %>%
    calculate_diffs()

wide_df <- dplyr::left_join(
    wide_df_supp %>%
        dplyr::select(
            year,
            abbrev,
            opioid_type,
            nhb_obs_rate,
            nhw_obs_rate,
            nhb_obs_se,
            nhw_obs_se
        ),
    wide_df_all %>%
        dplyr::select(-nhb_obs_rate,
                      -nhw_obs_rate,
                      -nhb_obs_se,
                      -nhw_obs_se)
)

## Save a private version with all data ----
readr::write_csv(wide_df_all,
                 here::here("data_private", "ratios_and_diffs_all.csv"))

## Make a public version with N<10 cells suppressed ----
readr::write_csv(wide_df,
                 here::here("data", "ratios_and_diffs_suppressed.csv"))
