library(tidyverse)
library(narcan)    # install_github("mkiang/narcan")
library(janitor)
library(here)
library(fs)
library(foreach)
library(config)
library(shiny)
library(knitr)
library(DT)
library(geofacet)
library(zoo)
source(here::here("code", "mk_nytimes.R"))

## Dataframe helpers ----

## Give it the path of a joinpoint result file (f_path). Returns a tibble.
import_jp <- function(f_path, ctypes = NULL) {
    readr::read_delim(f_path, delim = ";", col_types = ctypes) %>%
        janitor::clean_names(.)
}

## Give it a dataframe with opioid_type column.
## Returns a new tibble with factored opioid types (opioid_cat).
categorize_opioids <- function(df) {
    df %>%
        dplyr::mutate(opioid_cat = factor(
            opioid_type,
            levels = c("opioid", "natural", "heroin", "synth"),
            labels = c(
                "All opioids",
                "Natural and semi-synthetic opioids",
                "Heroin",
                "Synthetic opioids"
            ),
            ordered = TRUE
        ))
}

## Give it a dataframe with race_eth column.
## Returns a new tibble with factored race categories (race_cat).
categorize_race <- function(df) {
    df %>%
        dplyr::mutate(race_cat = factor(
            race_eth,
            levels = c("nhw", "nhb", "other"),
            labels = c("Non-Hispanic White",
                       "Non-Hispanic Black",
                       "Other"),
            ordered = TRUE
        ))
}



spread_col_by_race <- function(results_df, target_col, year_x) {
    target_col <- dplyr::enquo(target_col)
    results_df %>%
        dplyr::filter(year == year_x) %>%
        dplyr::select(race_eth, abbrev, opioid_type, !!target_col) %>%
        tidyr::spread(race_eth, !!target_col) %>%
        dplyr::rename(
            !!paste0("nhb_", dplyr::quo_name(target_col)) := nhb,
            !!paste0("nhw_", dplyr::quo_name(target_col)) := nhw
        )
}

## Used to convert joinpoint results from long to wide.
## Makes for calculating diffs and ratios easier. Give it a long results
## tibble and returns a wide results tibble.
reshape_to_yearly_wide <- function(results_df, year_x) {
    holder <- vector("list", NROW(year_x))
    for (i in 1:NROW(year_x)) {
        holder[[i]] <- results_df %>%
            spread_col_by_race(obs_rate, year_x = year_x[i]) %>%
            dplyr::left_join(
                results_df %>%
                    spread_col_by_race(obs_se, year_x = year_x[i]),
                by = c("abbrev", "opioid_type")
            ) %>%
            dplyr::left_join(
                results_df %>%
                    spread_col_by_race(model_rate, year_x = year_x[i]),
                by = c("abbrev", "opioid_type")
            ) %>%
            dplyr::left_join(
                results_df %>%
                    spread_col_by_race(apc, year_x = year_x[i]),
                by = c("abbrev", "opioid_type")
            ) %>%
            dplyr::left_join(
                results_df %>%
                    spread_col_by_race(slope_beta, year_x = year_x[i]),
                by = c("abbrev", "opioid_type")
            ) %>%
            dplyr::left_join(
                results_df %>%
                    spread_col_by_race(slope_se, year_x = year_x[i]),
                by = c("abbrev", "opioid_type")
            ) %>%
            dplyr::mutate(year = year_x[i])
    }
    dplyr::bind_rows(holder) %>%
        categorize_opioids() %>%
        dplyr::arrange(opioid_cat, abbrev, year) %>%
        dplyr::ungroup()
}

## Reshapes a widened results tibble (from reshape_to_yearly_wide()) into
## a longer one that is suitable for plotting.
reshape_wide_to_long <- function(sub_wide_ranked, target_col) {
    target_col <- dplyr::enquo(target_col)
    target_type <- dplyr::quo_name(target_col)
    target_x <- as.name(paste0("x_", target_type))
    target_ymin <- as.name(paste0(target_type, "_lower"))
    target_ymax <- as.name(paste0(target_type, "_upper"))
    target_sig <- as.name(paste0(target_type, "_sig"))
    
    sub_wide_ranked %>%
        dplyr::transmute(
            year = year,
            abbrev = abbrev,
            opioid_cat = opioid_cat,
            opioid_type = opioid_type,
            x_pos = !!target_x,
            y_pos = !!target_col,
            y_max = !!target_ymax,
            y_min = !!target_ymin,
            y_sig = !!target_sig,
            o_type = !!target_type
        )
}

## Give it widened results tibble (from reshape_to_yearly_wide()) and
## calculates the ratios and the variance (and bounds) of the ratios.
calculate_ratios <- function(wide_results) {
    ## See: http://www.stat.cmu.edu/~hseltman/files/ratio.pdf
    ## and Kendall’s Advanced Theory of Statistics, Arnold, London, 1998,
    ## 6th Edition, Volume 1, by Stuart & Ord
    wide_results %>%
        ## Ratios
        dplyr::mutate(
            obs_rr = nhb_obs_rate / nhw_obs_rate,
            model_rr = nhb_model_rate / nhw_model_rate,
            apc_ratio = nhb_apc / nhw_apc
        ) %>%
        ## Variances
        dplyr::mutate(
            obs_rr_var =
                (nhb_obs_rate ^ 2 / nhw_obs_rate ^ 2) *
                ((nhb_obs_se ^ 2 / nhb_obs_rate ^ 2) +
                     (nhw_obs_se ^ 2 / nhw_obs_rate ^ 2)
                ),
            apc_ratio_var =
                (nhb_apc ^ 2 / nhw_apc ^ 2) *
                ((
                    nhb_slope_se ^ 2 * exp(nhb_slope_beta) ^ 2 +
                        nhw_slope_se ^ 2 * exp(nhw_slope_beta) ^ 2
                ))
        ) %>%
        ## Bounds
        dplyr::mutate(
            obs_rr_lower = obs_rr - 1.96 * sqrt(obs_rr_var),
            obs_rr_upper = obs_rr + 1.96 * sqrt(obs_rr_var),
            apc_ratio_lower = apc_ratio - 1.96 * sqrt(apc_ratio_var),
            apc_ratio_upper = apc_ratio + 1.96 * sqrt(apc_ratio_var)
        )  %>%
        ## "Significance"
        dplyr::rowwise() %>%
        dplyr::mutate(
            obs_rr_sig = (!dplyr::between(1, obs_rr_lower, obs_rr_upper)) + 0,
            apc_ratio_sig = (!dplyr::between(1, apc_ratio_lower, apc_ratio_upper)) + 0
        ) %>%
        dplyr::arrange(abbrev, opioid_type) %>%
        dplyr::ungroup()
}

## Give it widened results tibble (from reshape_to_yearly_wide()) and
## calculates the diffs and the variance (and bounds) of the ratios.
calculate_diffs <- function(wide_results) {
    ## See Kendall’s Advanced Theory of Statistics, Arnold, London, 1998,
    ## 6th Edition, Volume 1, by Stuart & Ord
    wide_results %>%
        ## Diffs
        dplyr::mutate(
            obs_rd = nhb_obs_rate - nhw_obs_rate,
            model_rd = nhb_model_rate - nhw_model_rate,
            apc_diff = nhb_apc - nhw_apc
        ) %>%
        ## Variances
        dplyr::mutate(
            obs_rd_var = (nhb_obs_se ^ 2) + (nhw_obs_se ^ 2),
            apc_diff_var = (nhb_slope_se ^ 2 * (100 * exp(nhb_slope_beta)) ^ 2) +
                (nhw_slope_se ^ 2 * (100 * exp(nhw_slope_beta)) ^ 2)
        ) %>%
        ## Bounds
        dplyr::mutate(
            obs_rd_lower = obs_rd - 1.96 * sqrt(obs_rd_var),
            obs_rd_upper = obs_rd + 1.96 * sqrt(obs_rd_var),
            apc_diff_lower = apc_diff - 1.96 * sqrt(apc_diff_var),
            apc_diff_upper = apc_diff + 1.96 * sqrt(apc_diff_var)
        )  %>%
        ## "Significance"
        dplyr::rowwise() %>%
        dplyr::mutate(
            obs_rd_sig = (obs_rd_lower * obs_rd_upper > 0) + 0,
            apc_diff_sig = (apc_diff_lower * apc_diff_upper > 0) + 0
        ) %>%
        dplyr::arrange(abbrev, opioid_type) %>%
        dplyr::ungroup()
}

## Give it a dataframe with ratios and diffs.
## Returns a new tibble with the state ranks of each.
create_x_ranks <- function(sub_df) {
    sub_df %>%
        dplyr::group_by(year, opioid_cat, opioid_type) %>%
        dplyr::mutate(
            x_obs_rr = dplyr::dense_rank(obs_rr),
            x_obs_rd = dplyr::dense_rank(obs_rd),
            x_apc_ratio = dplyr::dense_rank(apc_ratio),
            x_apc_diff = dplyr::dense_rank(apc_diff)
        ) %>%
        dplyr::ungroup()
}


## Plotting helpers ----
## Returns two colors (for plotting).
return_two_colors <- function() {
    ## Just so it's easier to change colors across all plots
    c("#00468BCC", "#ED0000CC")
}

## Takes the modeled rate and converts it to wide format.
convert_modeled_rate_to_wide <- function(results_df) {
    results_df %>%
        dplyr::select(year, race_eth, abbrev, opioid_cat, model_rate) %>%
        dplyr::arrange(opioid_cat, race_eth, year) %>% 
        tidyr::spread(race_eth, model_rate) %>% 
        dplyr::rename(nhb_modeled = nhb, nhw_modeled = nhw)
}

convert_observed_rate_to_wide <- function(results_df) {
    results_df %>%
        dplyr::select(year, race_eth, abbrev, opioid_cat, obs_rate) %>%
        dplyr::arrange(opioid_cat, race_eth, year) %>% 
        tidyr::spread(race_eth, obs_rate) %>% 
        dplyr::rename(nhb_observed = nhb, nhw_observed = nhw)
}

convert_rates_to_wide <- function(results_df) {
    dplyr::left_join(
        convert_modeled_rate_to_wide(results_df), 
        convert_observed_rate_to_wide(results_df)
    )
}


## Caterpillar plots ----
## Given wide results, subsets to specified year and state.
subset_reshape_wide_results <-
    function(wide_results, year_x, abbrev_x) {
        sub_df <- wide_results %>%
            dplyr::filter(year == year_x) %>%
            dplyr::select(
                year,
                abbrev,
                opioid_cat,
                opioid_type,
                dplyr::starts_with("obs_"),
                dplyr::starts_with("apc_")
            ) %>%
            create_x_ranks()
        
        dplyr::bind_rows(
            reshape_wide_to_long(sub_df, obs_rr),
            reshape_wide_to_long(sub_df, obs_rd),
            reshape_wide_to_long(sub_df, apc_ratio),
            reshape_wide_to_long(sub_df, apc_diff)
        ) %>%
            flag_specific_state(abbrev_x = abbrev_x) %>%
            dplyr::mutate(o_cat = factor(
                o_type,
                levels = c("obs_rr", "obs_rd", "apc_ratio", "apc_diff"),
                labels = c(
                    "Black Rate / White Rate",
                    "Black Rate - White Rate",
                    "Black APC / White APC",
                    "Black APC - White APC"
                )
            ))
    }

## Plot the caterpillar plots for ratio or diffs (must specify).
plot_caterpillars <-
    function(wide_results, year_x, abbrev_x, type = "ratio") {
        plot_df <-
            subset_reshape_wide_results(wide_results, year_x, abbrev_x)
        if (type == "ratio") {
            o_types <- c("obs_rr", "apc_ratio")
            y_lab <- "Ratio (95% CI)"
            y_int <- 1
        } else if (type == "diff") {
            o_types <- c("obs_rd", "apc_diff")
            y_lab <- "Difference (95% CI)"
            y_int <- 0
        }
        
        ggplot2::ggplot(
            plot_df %>%
                dplyr::filter(o_type %in% o_types),
            ggplot2::aes(
                x = x_pos,
                y = y_pos,
                ymax = y_max,
                ymin = y_min,
                color = col_val,
                alpha = as.factor(y_sig)
            )
        ) +
            ggplot2::geom_hline(yintercept = y_int,
                                size = .75,
                                alpha = .8) +
            ggplot2::geom_errorbar(width = 0) +
            ggplot2::geom_point(ggplot2::aes(size = 1.025 * size_val),
                                color = "white") +
            ggplot2::geom_point() +
            ggplot2::scale_alpha_manual(NULL, values = c(.5, 1), guide = "none") +
            ggplot2::scale_size_identity(name = "Geographic Area") +
            ggplot2::scale_color_manual(name = "Geographic Area",
                                        drop = FALSE,
                                        values = return_two_colors()) +
            ggplot2::facet_grid(o_cat ~ opioid_cat,
                                scales = "free") +
            mk_nytimes(
                panel.border =
                    ggplot2::element_rect(
                        linetype = "solid",
                        fill = NA,
                        color = "grey75"
                    ),
                legend.position = "bottom",
                axis.text.x = ggplot2::element_blank()
            ) +
            ggplot2::scale_y_continuous(y_lab,
                                        expand = c(.01, 0)) +
            ggplot2::scale_x_continuous("Geographic Area (ranked by point estimate)",
                                        expand = c(0, .75))
    }

## Helper that will flag a specific state and provide values for that state.
## For example, changing the size or alpha to the state of focus.
flag_specific_state <-
    function(results_df,
             abbrev_x = "DC",
             alpha_interest = .9,
             alpha_other = .7,
             size_interest = 3,
             size_other = 2) {
        st_name <- return_st_name(abbrev_x)
        
        results_df %>%
            dplyr::mutate(
                alpha_val = ifelse(abbrev == abbrev_x, alpha_interest, alpha_other),
                col_val = ifelse(abbrev == abbrev_x, st_name, "Other Area"),
                size_val = ifelse(abbrev == abbrev_x, size_interest, size_other)
            ) %>%
            dplyr::mutate(col_val = factor(
                col_val,
                levels = c(st_name, "Other Area"),
                ordered = TRUE
            ))
    }

## Plot the mortality rate within a state from the joinpoint results tibble.
plot_state_mortality <- function(results_df,
                                 state_x = "DC",
                                 y_limit = c(NA, NA)) {
    sub_df <- results_df %>% dplyr::filter(abbrev == state_x)
    
    p1 <- ggplot2::ggplot(data =  sub_df) +
        ggplot2::geom_errorbar(
            ggplot2::aes(
                x = year,
                ymax = obs_rate + 1.96 * obs_se,
                ymin = obs_rate - 1.96 * obs_se,
                color = race_cat
            ),
            alpha = .4,
            width = .4
        ) +
        ggplot2::geom_point(ggplot2::aes(x = year,
                                         y = obs_rate,
                                         color = race_cat),
                            alpha = .4) +
        ggplot2::geom_line(size = 1.25,
                           ggplot2::aes(x = year, y = model_rate, color = race_cat)) +
        ggplot2::scale_x_continuous(
            NULL,
            breaks = seq(2000, 2015, 5),
            labels = c("'00", "'05", "'10", "'15"),
            expand = c(0, .25)
        ) +
        ggplot2::scale_y_continuous(paste("Age-standardized\nMortality Rate,",
                                          "per 100,000"),
                                    expand = c(0, 0)) +
        ggplot2::scale_color_manual(name = "Race/Ethnicity",
                                    values = rev(return_two_colors())) +
        mk_nytimes(
            panel.border =
                ggplot2::element_rect(
                    linetype = "solid",
                    fill = NA,
                    color = "grey75"
                ),
            legend.position = "bottom",
        ) +
        ggplot2::facet_grid( ~ opioid_cat)
    
    if (all(!is.na(y_limit))) {
        p1 + ggplot2::coord_cartesian(ylim = y_limit)
    } else {
        p1
    }
}

## Plot the black vs white estimates, highlighting a specified state.
plot_black_vs_white_estimates <-
    function(results_df,
             abbrev_x = "DC",
             y_limit = c(NA, NA),
             plot_obs = FALSE) {
        sub_df_wide_modeled <- dplyr::left_join(
            results_df %>%
                convert_modeled_rate_to_wide(),
            results_df %>%
                convert_observed_rate_to_wide() 
        ) %>%
            flag_specific_state(abbrev_x = abbrev_x) %>% 
            dplyr::arrange(opioid_cat, abbrev, year)
        
        p1 <- ggplot2::ggplot()
        
        if (plot_obs) {
            p1 <- p1 +
                ggplot2::geom_point(
                    data = sub_df_wide_modeled %>%
                        dplyr::filter(abbrev != abbrev_x),
                    ggplot2::aes(
                        x = nhw_observed,
                        y = nhb_observed,
                        group = abbrev,
                        alpha = alpha_val,
                        color = col_val,
                        size = col_val
                    ),
                    alpha = .2
                ) +
                ggplot2::geom_point(
                    data = sub_df_wide_modeled %>%
                        dplyr::filter(abbrev == abbrev_x),
                    ggplot2::aes(
                        x = nhw_observed,
                        y = nhb_observed,
                        group = abbrev,
                        alpha = alpha_val,
                        color = col_val,
                        size = col_val
                    ),
                    alpha = .5
                ) 
        }
        
        p1 <- p1 +
            ggplot2::geom_path(
                data = sub_df_wide_modeled %>%
                    dplyr::filter(abbrev != abbrev_x),
                ggplot2::aes(
                    x = nhw_modeled,
                    y = nhb_modeled,
                    group = abbrev,
                    alpha = alpha_val,
                    color = col_val,
                    size = col_val
                )
            ) +
            ggplot2::geom_path(
                data = sub_df_wide_modeled %>%
                    dplyr::filter(abbrev == abbrev_x),
                ggplot2::aes(
                    x = nhw_modeled,
                    y = nhb_modeled,
                    group = abbrev,
                    alpha = alpha_val,
                    color = col_val,
                    size = col_val
                )
            ) +
            ggplot2::facet_grid( ~ opioid_cat) +
            ggplot2::scale_alpha_identity(NULL) +
            ggplot2::scale_color_manual(NULL,
                                        drop = FALSE,
                                        values = return_two_colors()) +
            ggplot2::scale_size_manual(NULL,
                                       values = c(.8, .5),
                                       drop = FALSE) +
            ggplot2::scale_x_continuous("Smoothed Non-Hispanic White Mortality Rate (per 100,000)",
                                        expand = c(0, .1)) +
            ggplot2::scale_y_continuous("Smoothed Non-Hispanic Black\nMoratlity Rate (per 100,000)",
                                        expand = c(0, .1)) +
            mk_nytimes(
                panel.border =
                    ggplot2::element_rect(
                        linetype = "solid",
                        fill = NA,
                        color = "grey75"
                    ),
                legend.position = "bottom"
            )
        
        if (all(!is.na(y_limit))) {
            p1 + ggplot2::coord_cartesian(ylim = y_limit)
        } else {
            p1 
        }
    }

plot_ratios_by_year <- function(wide_results, year_x, abbrev_x) {
    sub_df <- wide_results %>%
        dplyr::filter(year == year_x) %>%
        flag_specific_state(abbrev_x = abbrev_x)
    
    ggplot2::ggplot(
        data = sub_df,
        ggplot2::aes(
            x = apc_ratio,
            y = obs_rr,
            size = size_val,
            group = abbrev,
            color = col_val
        )
    ) +
        ggplot2::geom_vline(xintercept = 1,
                            size = .75,
                            alpha = .8) +
        ggplot2::geom_hline(yintercept = 1,
                            size = .75,
                            alpha = .8) +
        ggplot2::geom_point(ggplot2::aes(size = 1.5 * size_val),
                            color = "white") +
        ggplot2::geom_point() +
        ggplot2::scale_size_identity(name = "Geographic Area") +
        ggplot2::scale_color_manual(name = "Geographic Area",
                                    drop = FALSE,
                                    values = return_two_colors()) +
        ggplot2::scale_x_continuous("Black APC / White APC",
                                    expand = c(0, 1)) +
        ggplot2::scale_y_continuous(
            "log(Black Mortality Rate / White Mortality Rate)",
            trans = "log",
            # breaks = c(1 / 6, 1 / 3, 1 / 1, 3, 6),
            labels = (function(x)
                sprintf("%0.2f", round(x, 2))),
            expand = c(0, .05)
        ) +
        ggplot2::facet_grid(~ opioid_cat,
                            scales = "free") +
        mk_nytimes(
            panel.border =
                ggplot2::element_rect(
                    linetype = "solid",
                    fill = NA,
                    color = "grey75"
                ),
            legend.position = "bottom"
        )
}

plot_combo_by_year <- function(wide_results, year_x, abbrev_x) {
    sub_df <- wide_results %>%
        dplyr::filter(year == year_x) %>%
        flag_specific_state(abbrev_x = abbrev_x)
    
    ggplot2::ggplot(
        data = sub_df,
        ggplot2::aes(
            x = apc_diff,
            y = obs_rr,
            size = size_val,
            group = abbrev,
            color = col_val
        )
    ) +
        ggplot2::geom_vline(xintercept = 1,
                            size = .75,
                            alpha = .8) +
        ggplot2::geom_hline(yintercept = 1,
                            size = .75,
                            alpha = .8) +
        ggplot2::geom_point(ggplot2::aes(size = 1.5 * size_val),
                            color = "white") +
        ggplot2::geom_point() +
        ggplot2::scale_size_identity(name = "Geographic Area") +
        ggplot2::scale_color_manual(name = "Geographic Area",
                                    drop = FALSE,
                                    values = return_two_colors()) +
        ggplot2::scale_x_continuous("Black APC - White APC",
                                    expand = c(0, 1)) +
        ggplot2::scale_y_continuous(
            "log(Black Mortality Rate / White Mortality Rate)",
            trans = "log",
            # breaks = c(1 / 6, 1 / 3, 1 / 1, 3, 6),
            labels = (function(x)
                sprintf("%0.2f", round(x, 2))),
            expand = c(0, .05)
        ) +
        ggplot2::facet_grid(~ opioid_cat,
                            scales = "free") +
        mk_nytimes(
            panel.border =
                ggplot2::element_rect(
                    linetype = "solid",
                    fill = NA,
                    color = "grey75"
                ),
            legend.position = "bottom"
        )
}

plot_diffs_by_year <- function(wide_results, year_x, abbrev_x) {
    sub_df <- wide_results %>%
        dplyr::filter(year == year_x) %>%
        flag_specific_state(abbrev_x = abbrev_x)
    
    ggplot2::ggplot(
        data = sub_df,
        ggplot2::aes(
            x = apc_diff,
            y = obs_rd,
            size = size_val,
            group = abbrev,
            color = col_val
        )
    ) +
        ggplot2::geom_vline(xintercept = 0,
                            size = .75,
                            alpha = .8) +
        ggplot2::geom_hline(yintercept = 0,
                            size = .75,
                            alpha = .8) +
        ggplot2::geom_point(ggplot2::aes(size = 1.5 * size_val),
                            color = "white") +
        ggplot2::geom_point() +
        ggplot2::scale_size_identity(name = "Geographic Area") +
        ggplot2::scale_color_manual(name = "Geographic Area",
                                    drop = FALSE,
                                    values = return_two_colors()) +
        ggplot2::scale_x_continuous("Black APC - White APC",
                                    expand = c(0, 1)) +
        ggplot2::scale_y_continuous("Black Mortality Rate - White Mortality Rate",
                                    expand = c(0, 1.5)) +
        ggplot2::facet_grid(~ opioid_cat,
                            scales = "free") +
        mk_nytimes(
            panel.border =
                ggplot2::element_rect(
                    linetype = "solid",
                    fill = NA,
                    color = "grey75"
                ),
            legend.position = "bottom"
        )
}

## Printing helpers ----
subset_to_model <-
    function(results_df, abbrev_x, opioid_x, race_x) {
        results_df %>%
            dplyr::filter(abbrev == abbrev_x,
                          opioid_type %in% opioid_x,
                          race_eth %in% race_x)
    }

subset_prediction_columns <- function(sub_df) {
    sub_df %>%
        dplyr::select(race_cat,
                      opioid_cat,
                      abbrev,
                      name,
                      year,
                      obs_rate,
                      obs_se,
                      model_rate)
}

print_prediction_table <-
    function(results_df, abbrev_x, opioid_x, race_x) {
        sub_df <- results_df %>%
            subset_to_model(abbrev_x = abbrev_x,
                            opioid_x = opioid_x,
                            race_x = race_x) %>%
            subset_prediction_columns()
        
        sub_df %>%
            dplyr::mutate(
                year = sprintf("%i", year),
                obs_rate = sprintf("%0.2f", round(obs_rate, 2)),
                obs_se = sprintf("%0.2f", round(obs_se, 2)),
                model_rate = sprintf("%0.2f", round(model_rate, 2))
            )
    }

subset_segment_columns <- function(sub_df) {
    sub_df %>%
        dplyr::select(
            race_cat,
            opioid_cat,
            abbrev,
            name,
            segment,
            dplyr::starts_with("apc"),
            dplyr::starts_with("jp_"),
            dplyr::starts_with("intercept"),
            dplyr::starts_with("slope")
        ) %>%
        dplyr::distinct()
}

print_beta_table <-
    function(results_df, abbrev_x, opioid_x, race_x) {
        sub_df <- results_df %>%
            subset_to_model(abbrev_x = abbrev_x,
                            opioid_x = opioid_x,
                            race_x = race_x) %>%
            subset_segment_columns()
        
        
        sub_df %>%
            dplyr::mutate(jp_year = ifelse(is.na(jp_year), 1999, jp_year)) %>%
            dplyr::transmute(
                race_cat = race_cat,
                opioid_cat = opioid_cat,
                abbrev = abbrev,
                name = name,
                segment = sprintf("%i", segment + 1),
                seg_start = sprintf("%i", jp_year),
                seg_ci = sprintf("(%i, %i)", jp_lower, jp_upper),
                apc = sprintf("%0.1f", round(apc, 1)),
                apc_95ci = sprintf("(%0.1f, %0.1f)",
                                   round(apc_lower, 1),
                                   round(apc_upper, 1)),
                apc_ttest = sprintf("%0.2f", round(apc_test_stat, 2)),
                apc_pval = dplyr::case_when(apc_pval < .001 ~ "<.001",
                                            TRUE ~ sprintf("%0.3f", round(apc_pval, 3))),
                intercept_beta_se = sprintf(
                    "%0.2f (%0.2f)",
                    round(intercept_beta, 2),
                    round(intercept_se, 2)
                ),
                intercept_ttest = sprintf("%0.2f",
                                          round(intercept_test_stat, 2)),
                intercept_pval = dplyr::case_when(
                    intercept_pval < .001 ~ "<.001",
                    TRUE ~ sprintf("%0.3f", round(intercept_pval, 3))
                ),
                slope_beta_se = sprintf("%0.2f (%0.2f)",
                                        round(slope_beta, 2),
                                        round(slope_se, 2)),
                slope_ttest = sprintf("%0.2f",
                                      round(slope_test_stat, 2)),
                slope_pval = dplyr::case_when(slope_pval < .001 ~ "<.001",
                                              TRUE ~ sprintf("%0.3f", round(slope_pval, 3))),
                slope_change_beta_se = sprintf(
                    "%0.2f (%0.2f)",
                    round(slope_change_beta, 2),
                    round(slope_change_se, 2)
                ),
                slope_change_ttest = sprintf("%0.2f", round(slope_change_test_stat, 2)),
                slope_change_pval = dplyr::case_when(
                    slope_change_pval < .001 ~ "<.001",
                    TRUE ~ sprintf("%0.3f", round(slope_change_pval, 3))
                )
            )
    }

subset_model_summary_columns <- function(sub_df) {
    sub_df %>%
        dplyr::select(
            race_cat,
            opioid_cat,
            abbrev,
            name,
            n_joins = f_model,
            dplyr::starts_with("aapc"),
            n_obs,
            n_param,
            n_suppressed,
            df,
            sse,
            mse
        ) %>% dplyr::distinct()
}

print_model_summary_table <-
    function(results_df, abbrev_x, opioid_x, race_x) {
        sub_df <- results_df %>%
            subset_to_model(abbrev_x = abbrev_x,
                            opioid_x = opioid_x,
                            race_x = race_x) %>%
            subset_model_summary_columns()
        
        sub_df %>%
            dplyr::transmute(
                race_cat = race_cat,
                opioid_cat = opioid_cat,
                abbrev = abbrev,
                name = name,
                n_joins = n_joins,
                aapc = sprintf("%0.1f", round(aapc, 1)),
                aapc_ci = sprintf(
                    "(%0.1f, %0.1f)",
                    round(aapc_lower, 1),
                    round(aapc_upper, 1)
                ),
                aapc_ttest = sprintf("%0.2f", round(aapc_test_stat, 2)),
                aapc_pval = dplyr::case_when(aapc_pval < .001 ~ "<.001",
                                             TRUE ~ sprintf("%0.3f", round(aapc_pval, 3))),
                df = sprintf("%i", df),
                n_param = sprintf("%i", n_param),
                sse = sprintf("%0.2f", round(sse, 2)),
                mse = sprintf("%0.2f", round(mse, 2)),
                n_obs = n_obs,
                n_suppressed = n_suppressed
            )
    }

print_ratios_table <- function(wide_results, year_x) {
    wide_results %>%
        dplyr::left_join(return_st_info()) %>%
        dplyr::filter(year == year_x) %>%
        dplyr::transmute(
            opioid_cat = opioid_cat,
            abbrev = abbrev,
            name = name,
            year = year,
            nhb_obs_rate = sprintf("%0.2f", round(nhb_obs_rate, 2)),
            nhw_obs_rate = sprintf("%0.2f", round(nhw_obs_rate, 2)),
            obs_rr = sprintf("%0.2f", round(obs_rr, 2)),
            obs_rr_ci = sprintf(
                "(%0.2f, %0.2f)",
                round(obs_rr_lower, 2),
                round(obs_rr_upper, 2)
            ),
            nhb_apc = sprintf("%0.1f%%", round(nhb_apc, 1)),
            nhw_apc = sprintf("%0.1f%%", round(nhw_apc, 1)),
            apc_ratio = sprintf("%0.2f", round(apc_ratio, 2)),
            apc_ratio_ci = sprintf(
                "(%0.2f, %0.2f)",
                round(apc_ratio_lower, 2),
                round(apc_ratio_upper, 2)
            )
        ) %>%
        dplyr::arrange(opioid_cat, abbrev)
}

print_diffs_table <- function(wide_results, year_x) {
    wide_results %>%
        dplyr::left_join(return_st_info()) %>%
        dplyr::filter(year == year_x) %>%
        dplyr::transmute(
            opioid_cat = opioid_cat,
            abbrev = abbrev,
            name = name,
            year = year,
            nhb_obs_rate = sprintf("%0.2f", round(nhb_obs_rate, 2)),
            nhw_obs_rate = sprintf("%0.2f", round(nhw_obs_rate, 2)),
            obs_rd = sprintf("%0.2f", round(obs_rd, 2)),
            obs_rd_ci = sprintf(
                "(%0.2f, %0.2f)",
                round(obs_rd_lower, 2),
                round(obs_rd_upper, 2)
            ),
            nhb_apc = sprintf("%0.1f%%", round(nhb_apc, 1)),
            nhw_apc = sprintf("%0.1f%%", round(nhw_apc, 1)),
            apc_diff = sprintf("%0.2f", round(apc_diff, 2)),
            apc_diff_ci = sprintf(
                "(%0.2f, %0.2f)",
                round(apc_diff_lower, 2),
                round(apc_diff_upper, 2)
            )
        ) %>%
        dplyr::arrange(opioid_cat, abbrev)
}

## Dataframe of state abbreviation, name, fips, and division mapping ----
## Early years of MCOD files use NCHS-specific state codes.
nchs_state_codes <-
    list(
        "AL" = "01",
        "AK" = "02",
        "AZ" = "03",
        "AR" = "04",
        "CA" = "05",
        "CO" = "06",
        "CT" = "07",
        "DE" = "08",
        "DC" = "09",
        "FL" = "10",
        "GA" = "11",
        "HI" = "12",
        "ID" = "13",
        "IL" = "14",
        "IN" = "15",
        "IA" = "16",
        "KS" = "17",
        "KY" = "18",
        "LA" = "19",
        "ME" = "20",
        "MD" = "21",
        "MA" = "22",
        "MI" = "23",
        "MN" = "24",
        "MS" = "25",
        "MO" = "26",
        "MT" = "27",
        "NE" = "28",
        "NV" = "29",
        "NH" = "30",
        "NJ" = "31",
        "NM" = "32",
        "NY" = "33",
        "NC" = "34",
        "ND" = "35",
        "OH" = "36",
        "OK" = "37",
        "OR" = "38",
        "PA" = "39",
        "RI" = "40",
        "SC" = "41",
        "SD" = "42",
        "TN" = "43",
        "TX" = "44",
        "UT" = "45",
        "VT" = "46",
        "VA" = "47",
        "WA" = "48",
        "WV" = "49",
        "WI" = "50",
        "WY" = "51"
    )

st_info <- dplyr::tibble(
    abbrev   = datasets::state.abb,
    division = as.character(datasets::state.division),
    st_lat   = datasets::state.center$y,
    st_lon   = datasets::state.center$x
) %>%
    ## We have to add DC because it's not a state
    dplyr::add_row(
        abbrev = "DC",
        division = "South Atlantic",
        st_lat = 38.9072,
        st_lon = -77.0369
    ) %>%
    dplyr::left_join(narcan::st_fips_map) %>%
    dplyr::rename(st_fips = fips) %>%
    dplyr::arrange(name) %>%
    dplyr::left_join(dplyr::tibble(
        abbrev = names(nchs_state_codes),
        nchs_fips = unlist(nchs_state_codes)
    ),
    by = "abbrev")
# rm(nchs_state_codes)

return_st_info <- function(subset_states = FALSE) {
    st_info <- dplyr::tibble(
        abbrev   = datasets::state.abb,
        division = as.character(datasets::state.division),
        st_lat   = datasets::state.center$y,
        st_lon   = datasets::state.center$x
    ) %>%
        ## We have to add DC because it's not a state
        dplyr::add_row(
            abbrev = "DC",
            division = "South Atlantic",
            st_lat = 38.9072,
            st_lon = -77.0369
        ) %>%
        dplyr::left_join(narcan::st_fips_map, by = "abbrev") %>%
        ## Add in whole US and NA
        dplyr::add_row(
            abbrev = "US",
            name = "zzWhole US",
            division = "Whole US",
            st_lat = 0,
            st_lon = 200
        ) %>%
        dplyr::add_row(
            abbrev = NA,
            name = "zzzUnknown State",
            division = "Unknown",
            st_lat = 0,
            st_lon = 199
        ) %>%
        dplyr::add_row(st_fips = fips) %>%
        dplyr::arrange(st_lon) %>%
        dplyr::mutate(
            lon_rank = dplyr::dense_rank(st_lon),
            alpha_rank = dplyr::dense_rank(name)
        ) %>%
        dplyr::mutate(name = gsub("zz|zzz", "", name))
    
    st_info <- st_info %>%
        dplyr::mutate(
            st_cat = factor(
                abbrev,
                levels = st_info %>%
                    dplyr::arrange(lon_rank) %>%
                    dplyr::pull(abbrev),
                ordered = TRUE
            ),
            name_cat = factor(
                name,
                levels = st_info %>%
                    dplyr::arrange(name) %>%
                    dplyr::pull(name),
                ordered = TRUE
            ),
            name_cat_alpha = factor(
                name,
                levels = st_info %>%
                    dplyr::arrange(alpha_rank) %>%
                    dplyr::pull(name),
                ordered = TRUE
            )
        )
    
    if (subset_states) {
        st_info %>%
            dplyr::arrange(name_cat) %>%
            dplyr::filter(
                abbrev %in% c(
                    "AL",
                    "AR",
                    "AZ",
                    "CA",
                    "CO",
                    "CT",
                    "DC",
                    "DE",
                    "FL",
                    "GA",
                    "IL",
                    "IN",
                    "KS",
                    "KY",
                    "LA",
                    "MA",
                    "MD",
                    "MI",
                    "MN",
                    "MO",
                    "MS",
                    "NC",
                    "NJ",
                    "NV",
                    "NY",
                    "OH",
                    "OK",
                    "PA",
                    "SC",
                    "TN",
                    "TX",
                    "VA",
                    "WA",
                    "WI"
                )
            )
    } else {
        st_info
    }
}

return_st_name <- function(abbrev_x = "DC") {
    return_st_info() %>%
        dplyr::filter(abbrev == abbrev_x) %>%
        dplyr::pull(name)
}
