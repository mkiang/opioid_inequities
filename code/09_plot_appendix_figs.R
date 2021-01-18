## Imports ----
library(tidyverse)
library(geofacet)
library(here)
source(here::here("code", "mk_nytimes.R"))
source(here::here("code", "utils.R"))

## Data ----
wide_results <- readr::read_csv(
    here::here("data", "ratios_and_diffs_suppressed.csv")
    ) %>%
    categorize_opioids()

results_df <- readr::read_csv(
    here::here("data", "joinpoint_estimates_suppressed.csv")
    )

results_df <- results_df %>%
    categorize_opioids() %>%
    categorize_race()

## Plot ratios ----
p1 <- plot_ratios_by_year(wide_results, 2018, "DC") +
    ggplot2::scale_y_continuous(
        "log(Black Mortality Rate / White Mortality Rate)",
        breaks = c(1 / 6, 1 / 3, 1, 3, 6),
        trans = "log",
        labels = c("1/6", "1/3", "1", "3", "6"),
        expand = c(0, .05)
    )
ggplot2::ggsave(
    "./plots/figS2_mortality_ratio_vs_apc_ratio.pdf",
    p1,
    width = 7.5,
    height = 3.5,
    device = grDevices::cairo_pdf,
    scale = 1.25
)
ggplot2::ggsave(
    "./plots/figS2_mortality_ratio_vs_apc_ratio.jpg",
    p1,
    width = 7.5,
    height = 3.5,
    dpi = 300,
    scale = 1.25
)

## Plot ratio caterpillars ----
p1 <- plot_caterpillars(wide_results, 2018, "DC", type = "ratio")
ggplot2::ggsave(
    "./plots/figS3_apc_mortality_caterpillars_rel.pdf",
    p1,
    width = 7.5,
    height = 3.5,
    device = grDevices::cairo_pdf,
    scale = 1.25
)
ggplot2::ggsave(
    "./plots/figS3_apc_mortality_caterpillars_rel.jpg",
    p1,
    width = 7.5,
    height = 3.5,
    dpi = 300,
    scale = 1.25
)

## Plot diff caterpillars ----
p1 <- plot_caterpillars(wide_results, 2018, "DC", type = "diff")
ggplot2::ggsave(
    "./plots/figS1_apc_mortality_caterpillars_diff.pdf",
    p1,
    width = 7.5,
    height = 3.5,
    device = grDevices::cairo_pdf,
    scale = 1.25
)
ggplot2::ggsave(
    "./plots/figS1_apc_mortality_caterpillars_diff.jpg",
    p1,
    width = 7.5,
    height = 3.5,
    dpi = 300,
    scale = 1.25
)


## Geographical facets
## All opioids
p1 <- ggplot2::ggplot(
    results_df %>% 
        dplyr::filter(opioid_type == "opioid"),
    ggplot2::aes(
        x = year,
        y = model_rate,
        color = race_cat,
        group = race_cat
    )
) + ggplot2::geom_line() +
    geofacet::facet_geo(~ abbrev) +
    ggplot2::scale_x_continuous(
        NULL,
        breaks = seq(2000, 2015, 5),
        labels = c("'00", "'05", "'10", "'15"),
        expand = c(0, .25)
    ) +
    ggplot2::scale_y_continuous(paste("Opioid-related Mortality Rate,",
                                      "per 100,000 (truncated)"),
                                expand = c(0, 0)) +
    ggplot2::scale_color_manual(name = "Race/Ethnicity",
                                values = rev(return_two_colors())) +
    ggplot2::coord_cartesian(ylim = c(0, 40)) +
    mk_nytimes(
        panel.border =
            ggplot2::element_rect(
                linetype = "solid",
                fill = NA,
                color = "grey75"
            ),
        legend.position = c(1, 0),
        legend.justification = c(1, 0),
    )
ggplot2::ggsave(
    here::here("plots", "figS4_opioid_mort_map.pdf"),
    p1,
    width = 9,
    height = 7,
    scale = 1,
    device = grDevices::cairo_pdf
)
ggplot2::ggsave(
    here::here("plots", "figS4_opioid_mort_map.jpg"),
    p1,
    width = 9,
    height = 7,
    scale = 1,
    dpi = 300
)

## Heroin
p1 <- ggplot2::ggplot(
    results_df %>% 
        dplyr::filter(opioid_type == "heroin"),
    ggplot2::aes(
        x = year,
        y = model_rate,
        color = race_cat,
        group = race_cat
    )
) + ggplot2::geom_line() +
    geofacet::facet_geo(~ abbrev) +
    ggplot2::scale_x_continuous(
        NULL,
        breaks = seq(2000, 2015, 5),
        labels = c("'00", "'05", "'10", "'15"),
        expand = c(0, .25)
    ) +
    ggplot2::scale_y_continuous(paste("Heroin-related Mortality Rate,",
                                      "per 100,000 (truncated)"),
                                expand = c(0, 0)) +
    ggplot2::scale_color_manual(name = "Race/Ethnicity",
                                values = rev(return_two_colors())) +
    ggplot2::coord_cartesian(ylim = c(0, 20)) +
    mk_nytimes(
        panel.border =
            ggplot2::element_rect(
                linetype = "solid",
                fill = NA,
                color = "grey75"
            ),
        legend.position = c(1, 0),
        legend.justification = c(1, 0),
    )
ggplot2::ggsave(
    here::here("plots", "figS5_heroin_mort_map.pdf"),
    p1,
    width = 9,
    height = 7,
    scale = 1,
    device = grDevices::cairo_pdf
)
ggplot2::ggsave(
    here::here("plots", "figS5_heroin_mort_map.jpg"),
    p1,
    width = 9,
    height = 7,
    scale = 1,
    dpi = 300
)

## Natural
p1 <- ggplot2::ggplot(
    results_df %>% 
        dplyr::filter(opioid_type == "natural"),
    ggplot2::aes(
        x = year,
        y = model_rate,
        color = race_cat,
        group = race_cat
    )
) + ggplot2::geom_line() +
    geofacet::facet_geo(~ abbrev) +
    ggplot2::scale_x_continuous(
        NULL,
        breaks = seq(2000, 2015, 5),
        labels = c("'00", "'05", "'10", "'15"),
        expand = c(0, .25)
    ) +
    ggplot2::scale_y_continuous(paste("Natural/semi-synthetic opioid-related Mortality Rate,",
                                      "per 100,000 (truncated)"),
                                expand = c(0, 0)) +
    ggplot2::scale_color_manual(name = "Race/Ethnicity",
                                values = rev(return_two_colors())) +
    ggplot2::coord_cartesian(ylim = c(0, 15)) +
    mk_nytimes(
        panel.border =
            ggplot2::element_rect(
                linetype = "solid",
                fill = NA,
                color = "grey75"
            ),
        legend.position = c(1, 0),
        legend.justification = c(1, 0),
    )
ggplot2::ggsave(
    here::here("plots", "figS6_natural_mort_map.pdf"),
    p1,
    width = 9,
    height = 7,
    scale = 1,
    device = grDevices::cairo_pdf
)
ggplot2::ggsave(
    here::here("plots", "figS6_natural_mort_map.jpg"),
    p1,
    width = 9,
    height = 7,
    scale = 1,
    dpi = 300
)

## Synthetic
p1 <- ggplot2::ggplot(
    results_df %>% 
        dplyr::filter(opioid_type == "synth"),
    ggplot2::aes(
        x = year,
        y = model_rate,
        color = race_cat,
        group = race_cat
    )
) + ggplot2::geom_line() +
    geofacet::facet_geo(~ abbrev) +
    ggplot2::scale_x_continuous(
        NULL,
        breaks = seq(2000, 2015, 5),
        labels = c("'00", "'05", "'10", "'15"),
        expand = c(0, .25)
    ) +
    ggplot2::scale_y_continuous(paste("Synthetic opioid-related Mortality Rate,",
                                      "per 100,000 (truncated)"),
                                expand = c(0, 0)) +
    ggplot2::scale_color_manual(name = "Race/Ethnicity",
                                values = rev(return_two_colors())) +
    ggplot2::coord_cartesian(ylim = c(0, 25)) +
    mk_nytimes(
        panel.border =
            ggplot2::element_rect(
                linetype = "solid",
                fill = NA,
                color = "grey75"
            ),
        legend.position = c(1, 0),
        legend.justification = c(1, 0),
    )
ggplot2::ggsave(
    here::here("plots", "figS7_synth_mort_map.pdf"),
    p1,
    width = 9,
    height = 7,
    scale = 1,
    device = grDevices::cairo_pdf
)
ggplot2::ggsave(
    here::here("plots", "figS7_synth_mort_map.jpg"),
    p1,
    width = 9,
    height = 7,
    scale = 1,
    dpi = 300
)
