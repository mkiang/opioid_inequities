## Imports ----
library(tidyverse)
library(geofacet)
library(patchwork)
library(here)
source(here::here("code", "mk_nytimes.R"))
source(here::here("code", "utils.R"))

## Data ----
results_df <-
    readr::read_csv(here::here("data", "joinpoint_estimates_suppressed.csv"))

results_df <- results_df %>%
    categorize_opioids() %>%
    categorize_race()

## Geographical facets ----
## All opioids, states with both NHB and NHW
p1_data <- results_df %>%
    dplyr::filter(
        opioid_type == "opioid",
        race_eth != "total",
        !(abbrev %in% c("ID", "MT", "WY", "SD", "ND", "ME"))
    ) %>% 
    select(
        year,
        st_fips,
        abbrev,
        name,
        race_eth,
        race_cat,
        opioid_type,
        opioid_cat,
        obs_rate,
        deaths,
        pop,
        model_rate
    )
p1 <- ggplot2::ggplot(p1_data,
                      ggplot2::aes(
                          x = year,
                          y = model_rate,
                          color = race_cat,
                          group = race_cat
                      )) + 
    ggplot2::geom_line(size = .8, alpha = .7) +
    geofacet::facet_geo( ~ abbrev) +
    ggplot2::scale_x_continuous(
        NULL,
        breaks = c(seq(1999, 2019, 5)),
        labels = c("'99", "", "", "", "'19"),
        expand = c(0, 0),
        limits = c(1998.9, 2019.1)
    ) +
    ggplot2::scale_y_continuous(
        paste("Opioid-related mortality rate,",
              "per 100,000 (truncated)"),
        expand = c(0, 0),
        breaks = c(0, 30, 60)
    ) +
    ggplot2::scale_color_manual(name = "Race/Ethnicity",
                                values = rev(return_two_colors())) +
    ggplot2::coord_cartesian(ylim = c(0, 60)) +
    mk_nytimes(
        panel.border =
            ggplot2::element_rect(
                linetype = "solid",
                fill = NA,
                color = "grey75"
            ),
        legend.position = c(1, 0),
        legend.justification = c(1, 0),
        axis.text.x = element_text(hjust = c(0, .5, .5, .5, 1))
    )

## Save ----
ggsave(
    here("plots", "fig01_map_all_opioids.pdf"),
    p1,
    width = 7.5,
    height = 5,
    scale = 1.3,
    device = cairo_pdf
)
ggsave(
    here("plots", "fig01_map_all_opioids.jpg"),
    p1,
    width = 7.5,
    height = 5,
    scale = 1.3,
    dpi = 300
)

write_csv(p1_data, here("output", "fig01_data.csv"))
