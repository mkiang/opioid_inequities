## Imports ----
library(tidyverse)
library(here)
source(here::here("code", "utils.R"))

## Data ----
results_df <-
    readr::read_csv(here::here("data", "joinpoint_estimates_suppressed.csv"))

# results_df <-
#     read_csv(here("data_private", "joinpoint_estimates_all.csv"))

results_df <- results_df %>%
    categorize_opioids() %>%
    categorize_race()

p2 <- plot_black_vs_white_estimates(results_df,
                                    abbrev_x = "DC",
                                    plot_obs = TRUE) +
    ggplot2::scale_x_continuous(
        "Modeled Non-Hispanic White Mortality Rate (per 100,000)",
        expand = c(0, .1),
        breaks = seq(0, 50, 10),
        labels = c(0, "", 20, "", 40, "")
    ) +
    ggplot2::scale_y_continuous(
        "Modeled Non-Hispanic Black\nMoratlity Rate (per 100,000)",
        expand = c(0, .1),
        breaks = seq(0, 60, 10),
        labels = c(0, "", 20, "", 40, "", 60)
    ) +
    ggplot2::coord_equal() +
    ggplot2::geom_abline(
        slope = 1,
        intercept = 0,
        linetype = "dotted",
        alpha = .8
    )

ggplot2::ggsave(
    "./plots/fig1_state_opioid_trajectories_modeled.pdf",
    p2,
    width = 7.5,
    height = 3.5,
    device = grDevices::cairo_pdf,
    scale = 1.25
)
ggplot2::ggsave(
    "./plots/fig1_state_opioid_trajectories_modeled.jpg",
    p2,
    width = 7.5,
    height = 3.5,
    dpi = 300,
    scale = 1.25
)

## Need to save the plot data for submission ----
sub_df_wide_modeled <- results_df %>%
    convert_modeled_rate_to_wide()
readr::write_csv(sub_df_wide_modeled, here::here("data", "fig1_data.csv"))
