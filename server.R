library(shiny)
library(DT)
library(here)
source(here("code", "utils.R"))

## Data ----
results_df <-
    readr::read_csv(here::here("data", "joinpoint_estimates_suppressed.csv"))

results_df <- results_df %>%
    categorize_opioids() %>%
    categorize_race()

## Wide data ----
wide_results <-
    readr::read_csv(here::here("data", "ratios_and_diffs_suppressed.csv")) %>% 
    categorize_opioids()

# Define server logic required to draw a histogram
shinyServer(function(input, output) {
    output$figure1 <- renderCachedPlot({
        plot_state_mortality(results_df, state_x = input$state_x)
    }, cacheKeyExpr = input$state_x,
    res = 100)
    
    output$figure2 <- renderCachedPlot({
        plot_black_vs_white_estimates(results_df, abbrev_x = input$state_x)
    },
    cacheKeyExpr = input$state_x,
    res = 100)
    
    output$figure5 <- renderCachedPlot({
        plot_ratios_by_year(wide_results,
                            abbrev_x = input$y_state_x,
                            year_x = input$year_x)
    }, cacheKeyExpr = c(input$y_state_x, input$year_x),
    res = 100)
    
    output$figure6 <- renderCachedPlot({
        plot_caterpillars(
            wide_results,
            abbrev_x = input$y_state_x,
            year_x = input$year_x,
            type = "ratio"
        )
    }, cacheKeyExpr = c(input$y_state_x, input$year_x),
    res = 100)
    
    output$figure3 <- renderCachedPlot({
        plot_diffs_by_year(wide_results,
                            abbrev_x = input$y_state_x,
                            year_x = input$year_x)
    }, cacheKeyExpr = c(input$y_state_x, input$year_x),
    res = 100)
    
    output$figure4 <- renderCachedPlot({
        plot_caterpillars(
            wide_results,
            abbrev_x = input$y_state_x,
            year_x = input$year_x,
            type = "diff"
        )
    }, cacheKeyExpr = c(input$y_state_x, input$year_x),
    res = 100)
    
    
    output$fig1title <- renderText({
        sprintf(
            "Figure S1. Opioid-related mortality in %s by race/ethnicity, 1999-2018",
            return_st_name(input$state_x)
        )
    })
    
    output$fig1caption <- renderText({
        paste(
            "<b>Figure S1.</b> This plot shows the joinpoint modeled",
            "opioid-related mortality rate (lines) for non-Hispanic White (orange)",
            sprintf(
                "and non-Hispanic Black (blue) residents of %s, by opioid-type,",
                return_st_name(input$state_x)
            ),
            "from 1999 to 2018. The points show the observed",
            "age-standardized opioid-related (and 95% confidence interval).",
            "Note that in accordance with the NCHS Data Use Agreement,",
            "observations based on fewer than ten deaths are suppressed from",
            "this plot."
        )
    })
    
    output$fig2title <- renderText({
        sprintf(
            "Figure 1. Racial/ethnic disparities in opioid-related mortality in %s vs others",
            return_st_name(input$state_x)
        )
    })
    
    output$fig2caption <- renderText({
        paste(
            "<b>Figure 1.</b> This plot shows the joinpoint modeled",
            "opioid-related mortality rate (lines) for non-Hispanic White (x-axis)",
            sprintf(
                "and non-Hispanic Black (y-axis) residents of %s (blue line)",
                return_st_name(input$state_x)
            ),
            "vs other geographies (orange), by opioid-type, from 1999 to 2018."
        )
    })
    
    output$fig3title <- renderText({
        sprintf(
            "Figure 3. Mortality rate difference vs APC difference, %i",
            input$year_x
        )
    })
    
    output$fig3caption <- renderText({
        paste(
            "<b>Figure 3.</b> This plot shows the difference in the estimated",
            "annual percent change (APC) on the x-axis and the difference in the", 
            "opioid-related mortality (y-axis)", 
            sprintf("for %i.", input$year_x), 
            "In both cases, the difference is defined as Black - White;", 
            "therefore, points to the right of the vertical black line indicate",
            "the opioid-related mortality rate in that state is increasing", 
            "more quickly in the non-Hispanic Black population than the", 
            "non-Hispanic White population. Similarly, points above", 
            "the horizontal black line indicate the non-Hispanic Black population", 
            "has a higher opioid-related mortality rate than the non-Hispanic", 
            "White population. For convenience, we highlight", 
            sprintf("%s in blue.", return_st_name(input$y_state_x)))
    })
    
    output$fig6title <- renderText({
        sprintf(
            "Appendix Figure 3. Mortality rate ratio and APC ratio, by state, %i",
            input$year_x
        )
    })
    
    output$fig5caption <- renderText({
        paste(
            "<b>Appendix Figure 2.</b> This plot shows the ratio of", 
            "opioid-related mortality (top row) and",
            "ratios of estimated annual percent change (APC; bottom row)", 
            sprintf("for %i.", input$year_x), 
            "Points above the horizontal black line indicate",
            "the opioid-related mortality rate or APC in that state is increasing", 
            "more quickly in the non-Hispanic Black population than the", 
            "non-Hispanic White population. States are ordered in increasing", 
            "value of the point estimates.", 
            "Vertical bars represent the 95% confidence interval. States with", 
            "95% confidence intervals that include the null value (1) are dimmed.", 
            "For convenience, we highlight", 
            sprintf("%s in blue.", return_st_name(input$y_state_x)))
    })
    
    output$fig5title <- renderText({
        sprintf(
            "Appendix Figure 1. Mortality rate ratio vs APC ratio, %i",
            input$year_x
        )
    })
    
    output$fig5caption <- renderText({
        paste(
            "<b>Appendix Figure 2.</b> This plot shows the ratio of estimated",
            "annual percent change (APC) on the x-axis and the ratio of", 
            "opioid-related mortality (y-axis)", 
            sprintf("for %i.", input$year_x), 
            "In both cases, the difference is defined as Black / White;", 
            "therefore, points to the right of the vertical black line indicate",
            "the opioid-related mortality rate in that state is increasing", 
            "more quickly in the non-Hispanic Black population than the", 
            "non-Hispanic White population. Similarly, points to above", 
            "the horizontal black line indicate the non-Hispanic black population", 
            "has a higher opioid-related mortality rate than the non-Hispanic", 
            "White population. For convenience, we highlight", 
            sprintf("%s in blue.", return_st_name(input$y_state_x)))
    })
    
    output$fig6title <- renderText({
        sprintf(
            "Appendix Figure 3. Mortality rate ratio and APC ratio, by state, %i",
            input$year_x
        )
    })
    
    output$fig6caption <- renderText({
        paste(
            "<b>Appendix Figure 3.</b> This plot shows the ratio in", 
            "opioid-related mortality (top row) and",
            "ratios of estimated annual percent change (APC; bottom row)", 
            sprintf("for %i.", input$year_x), 
            "Points above the horizontal black line indicate",
            "the opioid-related mortality rate or APC in that state is increasing", 
            "more quickly in the non-Hispanic Black population than the", 
            "non-Hispanic White population. States are ordered in increasing", 
            "value of the point estimates.", 
            "Vertical bars represent the 95% confidence interval. States with", 
            "95% confidence intervals that include the null value (1) are dimmed.", 
            "For convenience, we highlight", 
            sprintf("%s in blue.", return_st_name(input$y_state_x)))
    })
    
    output$pred_rates_table <- renderDataTable({
        print_prediction_table(
            results_df,
            input$state_x,
            opioid_x = c("heroin", "opioid", "synth", "natural"),
            race_x = c("nhw", "nhb")
        ) %>%
            arrange(abbrev, race_cat, opioid_cat, year) %>%
            select(-name) %>%
            datatable(
                rownames = FALSE,
                filter = "top",
                colnames = c(
                    "Race/Ethnicity",
                    "Opioid Type",
                    "Geography",
                    "Year",
                    "Observed Rate",
                    "Rate SE",
                    "Modeled Rate"
                ),
                caption = paste(
                    "Observed and modeled opioid-related mortality",
                    sprintf("(per 100,000) for %s",
                            return_st_name(input$state_x)),
                    "from 1999 to 2018, by opioid type and race/ethnicity. "
                )
            )
    })
    
    output$model_coefs_table <- renderDataTable({
        print_beta_table(
            results_df,
            input$state_x,
            opioid_x = c("heroin", "opioid", "synth", "natural"),
            race_x = c("nhw", "nhb")
        ) %>%
            arrange(abbrev, race_cat, opioid_cat) %>%
            select(-name) %>%
            datatable(
                rownames = FALSE,
                filter = "top",
                colnames = c(
                    "Race/Ethnicity",
                    "Opioid Type",
                    "Geography",
                    "Segment Number", 
                    "Segment Start",
                    "Segment 95% CI",
                    "APC",
                    "APC 95% CI", 
                    "APC Test Stat.", 
                    "APC P-Value", 
                    "Intercept (SE)", 
                    "Intercept Test Stat.", 
                    "Intercept P-Value", 
                    "Slope Coef. (SE)",
                    "Slope Test Stat.", 
                    "Slope P-Value", 
                    "Change in Slope Coef. (SE)", 
                    "Change in Slope Test Stat.", 
                    "Change in Slope P-Value"
                ),
                caption = paste(
                    "Coefficients of all joinpoint models for ",
                    sprintf("%s.", return_st_name(input$state_x)),
                    "Notes: A joinpoint model estimates a trend as a set", 
                    "of (joined) linear segments. Each segment has its own", 
                    "intercept and slope. In addition, the model tests if", 
                    "the slope before and after a join is statistically", 
                    "significant.",
                    "When a slope coefficient is exponentiated, it can", 
                    "be interpreted as the annual percent change for that", 
                    "segment."
                )
            )
    })
    
    output$model_summary_stats <- renderDataTable({
        print_model_summary_table(
            results_df,
            input$state_x,
            opioid_x = c("heroin", "opioid", "synth", "natural"),
            race_x = c("nhw", "nhb")
        ) %>%
            arrange(abbrev, race_cat, opioid_cat) %>%
            select(-name) %>%
            datatable(
                rownames = FALSE,
                filter = "top",
                colnames = c(
                    "Race/Ethnicity",
                    "Opioid Type",
                    "Geography",
                    "Num. Joinpoints",
                    "AAPC",
                    "AAPC 95% CI",
                    "AAPC Test Stat.",
                    "AAPC P-Value", 
                    "DF", 
                    "Num. Parameters", 
                    "SSE", 
                    "MSE", 
                    "Num. Observations", 
                    "Num. Suppressed"
                ),
                caption = paste(
                    "Summary statistics of all joinpoint models for ",
                    sprintf("%s.", return_st_name(input$state_x)),
                    "Notes: AAPC is the average annual percent change", 
                    "or the average percent increase from 1999 to 2018. It", 
                    "is the weighted average of the annual percent change in",
                    "each segment where the weights are the number of years", 
                    "in the segment.",
                    "SSE and MSE are the sum of squared errors and mean", 
                    "squared error, respectively.",
                    "The NCHS Data Use Agreement requires all rates based", 
                    "on fewer than ten observations be suppressed. Therefore,", 
                    "when a model has one suppressed observation, the MSE and",
                    "SSE are suppressed to prevent calculating the missing", 
                    "rate. The number of suppressed observations are shown", 
                    "in the final column."
                )
            )
    })
    
    output$ratios_table <- renderDataTable({
        print_ratios_table(wide_results, input$year_x) %>%
            select(-name) %>%
            datatable(
                rownames = FALSE,
                filter = "top",
                colnames = c(
                    "Opioid Type",
                    "Geography",
                    "Year",
                    "NHB Rate",
                    "NHW Rate",
                    "Rate Ratio",
                    "Rate Ratio 95% CI",
                    "NHB APC",
                    "NHW APC",
                    "APC Ratio",
                    "APC Ratio 95% CI"
                ),
                caption = paste(
                    "Mortality and APC ratios, by state and opioid type, for",
                    sprintf("%i.", input$year_x)
                )
            )
    })
    
    output$diffs_table <- renderDataTable({
        print_diffs_table(wide_results, input$year_x) %>%
            select(-name) %>%
            datatable(
                rownames = FALSE,
                filter = "top",
                colnames = c(
                    "Opioid Type",
                    "Geography",
                    "Year",
                    "NHB Rate",
                    "NHW Rate",
                    "Rate Difference",
                    "Rate Difference 95% CI",
                    "NHB APC",
                    "NHW APC",
                    "APC Difference",
                    "APC Difference 95% CI"
                ),
                caption = paste(
                    "Mortality and APC differences, by state and opioid type, for",
                    sprintf("%i.", input$year_x)
                )
            )
    })
    
})
