## Imports ----
library(tidyverse)
library(here)
source(here::here("code", "mk_nytimes.R"))
source(here::here("code", "utils.R"))

## Data ----
wide_results <-
    readr::read_csv(here::here("data", "ratios_and_diffs_suppressed.csv")) %>%
    categorize_opioids()

## Plot diffs ----
p1 <- plot_diffs_by_year(wide_results, 2018, "DC")
ggplot2::ggsave(
    "./plots/fig3_mortality_diff_vs_apc_diff.pdf",
    p1,
    width = 7.5,
    height = 3.5,
    device = grDevices::cairo_pdf,
    scale = 1.25
)
ggplot2::ggsave(
    "./plots/fig3_mortality_diff_vs_apc_diff.jpg",
    p1,
    width = 7.5,
    height = 3.5,
    dpi = 300,
    scale = 1.25
)
## Need to save the plot data for submission ----
sub_df <- wide_results %>%
    dplyr::filter(year == 2018) %>%
    flag_specific_state(abbrev_x = "DC") %>%
    dplyr::select(opioid_type, opioid_cat, year, abbrev, obs_rd, apc_diff)
readr::write_csv(sub_df, here::here("data", "fig3_data.csv"))
