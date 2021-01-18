## Imports ----
library(tidyverse)
library(here)
source(here::here("code", "utils.R"))

## Data ----
results_df <-
    readr::read_csv(here::here("data", "joinpoint_estimates_suppressed.csv"))

results_df <- results_df %>%
    categorize_opioids() %>%
    categorize_race()

p1 <- plot_state_mortality(results_df, "DC", c(0, 70))

ggplot2::ggsave(
    "./plots/fig2_1999_2018_opioid_mortality.jpg",
    p1,
    dpi = 300,
    width = 8,
    height = 3,
    scale = 1.25
)
ggplot2::ggsave(
    "./plots/fig2_1999_2018_opioid_mortality.pdf",
    p1,
    width = 8,
    height = 3,
    device = grDevices::cairo_pdf,
    scale = 1.25
)

## Need to save the plot data for submission ----
sub_df <- results_df %>%
    dplyr::filter(abbrev == "DC") %>%
    dplyr::mutate(obs_lower = obs_rate - 1.96 * obs_se,
                  obs_upper = obs_rate + 1.96 * obs_se) %>%
    dplyr::select(
        year,
        race_eth,
        race_cat,
        opioid_type,
        opioid_cat,
        abbrev,
        obs_rate,
        obs_lower,
        obs_upper,
        model_rate
    )
readr::write_csv(sub_df, here::here("data", "fig2_data.csv"))
