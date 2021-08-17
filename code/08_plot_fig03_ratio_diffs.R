## Imports ----
library(tidyverse)
library(here)
source(here("code", "mk_nytimes.R"))

## Data ----
wide_df <- readRDS(here("data", "ratios_and_diffs_suppressed.RDS"))
    
## Subset a bit to clean it up
sub_df <- wide_df %>%
    filter(year == 2019,
           opioid_type %in% c("opioid", "heroin", "natural", "synth"),
           st_fips != "11") %>%
    mutate(big_point = ifelse(st_fips %in% c("12099", "11001"), abbrev, "Other area"))

## Save only relevant columns
p1_data <- sub_df %>%
    select(
        year,
        opioid_type,
        opioid_cat,
        st_fips,
        name,
        abbrev,
        obs_rd,
        obs_rr,
        big_point,
        obs_rd_lower,
        obs_rd_upper,
        obs_rr_lower,
        obs_rr_upper
    )

## Plot ----
p1 <- ggplot(p1_data,
       aes(
           x = obs_rd,
           y = obs_rr,
           size = big_point,
           color = big_point,
           alpha = big_point
       )) +
    geom_vline(xintercept = 0,
               linetype = "dotted",
               alpha = .8) +
    geom_hline(yintercept = 1,
               linetype = "dotted",
               alpha = .8) +
    geom_point() +
    scale_y_continuous(
        "Rate ratio (Black / white)",
        trans = "log",
        breaks = c(1 / 10, 1 / 5, 1 / 2, 1, 2, 5, 10),
        labels = c("1/10", "1/5", "1/2", "1", "2", "5", "10")
    ) +
    scale_x_continuous("Rate difference, per 100,000 population (Black - white)") +
    scale_size_manual("Area", values = c(2.5, 1.25, 2.5)) +
    scale_alpha_manual("Area", values = c(1, .7, 1)) +
    scale_color_brewer("Area", palette = "Dark2") +
    facet_wrap( ~ opioid_cat, nrow = 1) +
    mk_nytimes(
        legend.position = "right",
        panel.border =
            ggplot2::element_rect(
                linetype = "solid",
                fill = NA,
                color = "grey75"
            )
    )

## Save ----
ggsave(
    here("plots", "fig03_ratio_diffs_2019.pdf"),
    p1,
    width = 11,
    height = 4,
    scale = 1,
    device = cairo_pdf
)
ggsave(
    here("plots", "fig03_ratio_diffs_2019.jpg"),
    p1,
    width = 11,
    height = 4,
    scale = 1,
    dpi = 300
)

write_csv(p1_data, here("output", "fig03_data.csv"))
