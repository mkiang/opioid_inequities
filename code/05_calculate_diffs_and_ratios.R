## Imports ----
library(tidyverse)
library(here)
source(here::here("code", "mk_nytimes.R"))
source(here::here("code", "utils.R"))

## Data ----
long_df_all <- readr::read_csv(here::here("data_private", "joinpoint_estimates_all.csv")) %>%
    select(year:obs_se) %>% 
    select(-pop) %>% 
    filter(race_eth != "total")

## Pivot to wide
wide_df_all <- long_df_all %>%
    pivot_wider(
        id_cols = c(year, st_fips, abbrev, name, opioid_type),
        names_from = race_eth,
        values_from = obs_rate:obs_se
    ) %>% 
    categorize_opioids() %>% 
    filter(deaths_nhb > 10, 
           deaths_nhw > 10)

## Calculate ratios and diffs ----
wide_df_all <- wide_df_all %>%
    ## Ratio
    mutate(obs_rr = obs_rate_nhb / obs_rate_nhw,
           obs_rd = obs_rate_nhb - obs_rate_nhw) %>%
    ## Variance
    mutate(
        obs_rr_se = (1/obs_rate_nhb)^2 * obs_se_nhb^2 + 
            (1 / obs_rate_nhw)^2 * obs_se_nhw^2,
        obs_rd_se = sqrt(obs_se_nhb^2 + obs_se_nhw^2)
    ) %>%
    ## Bounds
    dplyr::mutate(
        obs_rr_lower = obs_rr - 1.96 * obs_rr_se,
        obs_rr_upper = obs_rr + 1.96 * obs_rr_se,
        obs_rd_lower = obs_rd - 1.96 * obs_rd_se,
        obs_rd_upper = obs_rd + 1.96 * obs_rd_se
    )  %>%
    ## "Significance"
    dplyr::rowwise() %>%
    dplyr::mutate(
        obs_rr_sig = (!dplyr::between(1, obs_rr_lower, obs_rr_upper)) + 0,
        obs_rd_sig = (!dplyr::between(1, obs_rr_lower, obs_rr_upper)) + 0
    ) %>%
    dplyr::arrange(st_fips, opioid_type) %>%
    dplyr::ungroup()

## Make a public version with N<10 cells suppressed ----
readr::write_csv(wide_df_all,
                 here::here("data", "ratios_and_diffs_suppressed.csv"))
saveRDS(wide_df_all, here::here("data", "ratios_and_diffs_suppressed.RDS"))
