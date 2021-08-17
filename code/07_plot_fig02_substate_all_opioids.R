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

top_16_substate <- results_df %>% 
    filter(nchar(abbrev) > 2, opioid_type == "opioid", race_eth != "total") %>% 
    arrange(desc(model_rate)) %>% 
    select(name, abbrev, st_fips) %>%
    distinct() %>% 
    slice(1:16) %>% 
    pull(st_fips)

p2_all <- plot_substate_facets(
    results_df %>%
        dplyr::filter(st_fips %in% top_16_substate),
    opioid_x = "opioid",
    include_total_pop = FALSE,
    return_data = TRUE
)

p2 <- p2_all$plot +
    facet_wrap( ~ abbrev,
                nrow = 2,
                labeller = label_wrap_gen(width = 16)) +
    ggplot2::scale_y_continuous(
        paste("Opioid-related mortality rate,",
              "per 100,000 (truncated)"),
        expand = c(0, 0),
        breaks = c(0, 30, 60, 90)
    ) 

p2_data <- p2_all$data

## Save ----
ggsave(
    here("plots", "fig02_substate_all_opioids.pdf"),
    p2,
    width = 7.5,
    height = 3,
    scale = 1.3,
    device = cairo_pdf
)
ggsave(
    here("plots", "fig02_substate_all_opioids.jpg"),
    p2,
    width = 7.5,
    height = 3,
    scale = 1.3,
    dpi = 300
)

write_csv(p2_data, here("output", "fig02_data.csv"))
