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

loop_df <- tribble(
    ~opioid_x, ~y_label, ~file_name,
    "opioid", "Opioid", "figS01_map_all_opioids",
    "natural", "Natural/semi-synthetic opioid", "figS02_map_natural_opioids",
    "heroin", "Heroin", "figS03_map_heroin",
    "synth", "Synthetic opioid", "figS04_map_synthetic_opioid",
    "poly_opioid", "Multiopioid", "figS05_map_polyopioid",
    "single_heroin", "Only heroin", "figS06_map_single_heroin",
    "single_natural", "Only natural/semi-synthetic opioid", "figS07_map_single_natural", 
    "single_synth", "Only synthetic opioid", "figS08_map_single_synth"
)

for (i in 1:NROW(loop_df)) {
    px_all <- plot_geofacet(
        results_df,
        opioid_x = loop_df$opioid_x[i],
        include_total_pop = TRUE,
        return_data = TRUE
    )
    
    px_plot <- px_all$plot + 
        ggplot2::scale_y_continuous(
            sprintf("%s-related mortality rate per 100,000", loop_df$y_label[i]),
            expand = c(0, 0)
        ) 
    
    ## Save ----
    ggsave(
        here("plots", paste0(loop_df$file_name[i], ".pdf")),
        px_plot,
        width = 7.5,
        height = 5,
        scale = 1.3,
        device = cairo_pdf
    )
    ggsave(
        here("plots",  paste0(loop_df$file_name[i], ".jpg")),
        px_plot,
        width = 7.5,
        height = 5,
        scale = 1.3,
        dpi = 300
    )
    
    write_csv(px_all$data, 
              here("output", paste0(substr(loop_df$file_name[i], 1, 6), "_data.csv")))
    
}
