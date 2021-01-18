## Create a working dataframe with all information ---
## NOTE: Follow the instructions in the README and use the provided joinpoint
## session file. This assumes you have exported all joinpoint results as
## text files in the `./joinpoint_analyses` folder and they are the
## standard names. Change accordingly.

## Imports ----
library(tidyverse)
library(here)
source(here::here("code", "utils.R"))

## First get the observed and modeled rates ----
working_df <-
    import_jp(here::here("joinpoint_analyses",
                         "age_std_rates_long.data.txt"),
              ctypes = "cccinnncii") %>%
    dplyr::select(
        year,
        race_eth,
        abbrev,
        opioid_type,
        obs_rate = rate,
        obs_se = standard_error,
        model_rate = model,
        f_model = final_selected_model,
        flag
    ) %>%
    dplyr::mutate(flag = ifelse(is.na(flag), 0, flag))

## Add in the APC of each segment ----
working_df <- working_df %>%
    dplyr::left_join(
        import_jp(
            here::here("joinpoint_analyses",
                       "age_std_rates_long.apc.txt")
        ) %>%
            dplyr::select(
                race_eth,
                abbrev,
                opioid_type,
                f_model = model,
                segment,
                year = segment_start,
                apc,
                apc_lower = apc_95_percent_lcl,
                apc_upper = apc_95_percent_ucl,
                apc_test_stat = test_statistic,
                apc_pval = p_value
            )
    )

working_df <- working_df %>%
    dplyr::group_by(race_eth, abbrev, opioid_type) %>%
    dplyr::arrange(year, .by_group = TRUE) %>%
    dplyr::mutate_at(dplyr::vars(segment:apc_pval),
                     (function(x)
                         na.locf(x))) %>%
    dplyr::ungroup()


## Add in the AAPC overall ----
working_df <- working_df %>%
    dplyr::left_join(
        import_jp(
            here::here("joinpoint_analyses",
                       "age_std_rates_long.aapc.txt")
        ) %>%
            dplyr::select(
                race_eth,
                abbrev,
                opioid_type,
                f_model = joinpoint_model,
                aapc,
                aapc_lower = aapc_c_i_low,
                aapc_upper = aapc_c_i_high,
                aapc_test_stat = test_statistic,
                aapc_pval = p_value,
                start_year = start_obs,
                end_year = end_obs
            )
    )

## Add in JP model estimates ----
working_df <- working_df %>%
    dplyr::left_join(
        import_jp(
            here::here(
                "joinpoint_analyses",
                "age_std_rates_long.modelestimates.txt"
            )
        ) %>%
            dplyr::select(
                race_eth,
                abbrev,
                opioid_type,
                segment,
                n_obs = number_obs,
                n_param = number_param,
                df,
                sse,
                mse,
                jp_year = joinpoint,
                jp_lower = joinpoint_95_percent_lcl,
                jp_upper = joinpoint_95_percent_ucl,
                intercept_beta = intercept_estimate,
                intercept_se = intercept_std_error,
                intercept_test_stat = intercept_test_statistic,
                intercept_pval = intercept_p_value,
                slope_beta = slope_estimate,
                slope_se = slope_std_error,
                slope_test_stat = slope_test_statistic,
                slope_pval = slope_p_value,
                slope_change_beta = slope_chg_estimate,
                slope_change_se = slope_chg_std_error,
                slope_change_test_stat = slope_chg_test_statistic,
                slope_change_pval = slope_chg_p_value
            )
    ) %>%
    dplyr::arrange(race_eth, abbrev, opioid_type, year)

## Add in number of deaths ----
full_df <- readr::read_csv(here::here("joinpoint_analyses", "age_std_rates_long.csv"))

full_df <- full_df %>%
    dplyr::group_by(race_eth, abbrev, opioid_type) %>%
    dplyr::mutate(n_suppressed = sum(deaths < 10)) %>%
    dplyr::arrange(opioid_type, race_eth, abbrev, year)

working_df <- working_df %>%
    dplyr::left_join(
        full_df %>%
            dplyr::select(
                year,
                abbrev,
                race_eth,
                name,
                opioid_type,
                deaths,
                pop,
                n_suppressed
            )
    ) %>% 
    dplyr::arrange(opioid_type, race_eth, abbrev, year)

## Save a private version with all data ----
readr::write_csv(working_df,
                 here::here("data_private", "joinpoint_estimates_all.csv"))

## Make a public version with N<10 cells suppressed ----
working_df <- working_df %>%
    dplyr::mutate(deaths = ifelse(deaths < 10, NA, deaths)) %>%
    dplyr::mutate(obs_rate = ifelse(is.na(deaths), NA, obs_rate),
                  obs_se = ifelse(is.na(deaths), NA, obs_se)) %>%
    dplyr::mutate(mse = ifelse(n_suppressed == 1, NA, mse),
                  sse = ifelse(n_suppressed == 1, NA, sse))
readr::write_csv(working_df,
                 here::here("data", "joinpoint_estimates_suppressed.csv"))
