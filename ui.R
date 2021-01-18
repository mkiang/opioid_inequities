## Imports ----
library(shiny)

## Make the UI
shinyUI(navbarPage("Disproportionate opioid deaths", 
    tabPanel(
        "State-specific results",
        ## Just for a little vertical space
        br(),
        
        ## Top row
        fluidRow(
            ## Title/Author panel
            column(width = 4,
                   wellPanel(
                       tags$blockquote(em(paper_title)),
                       tags$ul(mvk_tag, act_tag, mja_tag, dr_tag, sb_tag)
                   )),
            ## Information panel
            column(
                width = 8,
                h4("Modeling state-specific opioid mortality"),
                p(paper_blurb1),
                more_info1
            )
        ),
        
        ## Select a state ----
        hr(),
        fluidRow(
            column(
                width = 4,
                offset = 4,
                align = "center",
                selectInput(
                    "state_x",
                    label = "Select Geography:",
                    choices = st_names,
                    selected = "DC"
                )
            )
            ),
        
        ## Figure S1
        hr(),
        fluidRow(column(
            width = 12,
            h3(textOutput("fig2title")),
            align = "center",
            plotOutput("figure2", height = "300px")
        )),
        fluidRow(column(
            width = 10,
            offset = 1,
            align = "center",
            htmlOutput("fig2caption")
        )),
        br(),
        ## Figure 1
        fluidRow(column(
            width = 12,
            h3(textOutput("fig1title")),
            align = "center",
            plotOutput("figure1", height = "300px")
        )),
        fluidRow(column(
            width = 10,
            offset = 1,
            align = "center",
            htmlOutput("fig1caption")
        )),
        hr(),
        br(),
        
        ## Predicted rates table
        fluidRow(
            column(
                width = 10,
                offset = 1,
                align = "center",
                h3("Observed and Modeled Rates"),
                DT::dataTableOutput("pred_rates_table")
            )
        ),
        
        ## Model coefficients table
        fluidRow(
            column(
                width = 10,
                offset = 1,
                align = "center",
                h3("Model Coefficients Table"),
                DT::dataTableOutput("model_coefs_table")
            )
        ),
        
        ## Model summary stats table
        fluidRow(
            column(
                width = 10,
                offset = 1,
                align = "center",
                h3("Model summary statistics"),
                DT::dataTableOutput("model_summary_stats")
            )
        ),
        
        ## Footer ====
        fluidRow(column(
            width = 12,
            align = 'center',
            footer_tag
        ),
        br())
    ),
    tabPanel(
        "Year-specific results",
        ## Just for a little vertical space
        br(),
        
        ## Top row
        fluidRow(
            ## Title/Author panel
            column(width = 4,
                   wellPanel(
                       tags$blockquote(em(paper_title)),
                       tags$ul(mvk_tag, act_tag, mja_tag, dr_tag, sb_tag)
                   )),
            ## Information panel
            column(
                width = 8,
                h4("Examining state/year results"),
                p(paper_blurb1),
                more_info1
            )
        ),
        
        ## Select a state ----
        hr(),
        fluidRow(
            column(
                width = 3,
                offset = 3,
                align = "center",
                selectInput(
                    "y_state_x",
                    label = "Select Geography:",
                    choices = st_names,
                    selected = "DC"
                )
            ),
            column(
                width = 3,
                align = "center",
                sliderInput(
                    "year_x",
                    label = "Select Year:",
                    min = 1999,
                    max = 2018,
                    value = 2018,
                    step = 1,
                    round = TRUE,
                    sep = ""
                )
            )
        ),
        
        ## Figure 3
        hr(),
        fluidRow(column(
            width = 12,
            h3(textOutput("fig3title")),
            align = "center",
            plotOutput("figure3", height = "300px")
        )),
        fluidRow(column(
            width = 10,
            offset = 1,
            align = "center",
            htmlOutput("fig3caption")
        )),
        hr(),
        br(),
        ## Table
        fluidRow(
            column(
                width = 10,
                offset = 1,
                align = "center",
                h3("Estimates of Mortality Rate Difference and APC Difference"),
                DT::dataTableOutput("diffs_table")
            )
        ),
        hr(),
        br(),
        ## Figure 4
        fluidRow(column(
            width = 12,
            h3(textOutput("fig4title")),
            align = "center",
            plotOutput("figure4", height = "600px")
        )),
        fluidRow(column(
            width = 10,
            offset = 1,
            align = "center",
            htmlOutput("fig4caption")
        )),
        br(),
        ## Figure 5
        fluidRow(column(
            width = 12,
            h3(textOutput("fig5title")),
            align = "center",
            plotOutput("figure5", height = "300px")
        )),
        fluidRow(column(
            width = 10,
            offset = 1,
            align = "center",
            htmlOutput("fig5caption")
        )),
        br(),
        ## Figure 6
        fluidRow(column(
            width = 12,
            h3(textOutput("fig6title")),
            align = "center",
            plotOutput("figure6", height = "600px")
        )),
        fluidRow(column(
            width = 10,
            offset = 1,
            align = "center",
            htmlOutput("fig6caption")
        )),
        hr(),
        ## Table
        fluidRow(
            column(
                width = 10,
                offset = 1,
                align = "center",
                h3("Estimates of Rate Ratios and APC Ratio"),
                DT::dataTableOutput("ratios_table")
            )
        ),
        hr(),
        br(), 
        br(),
        
        ## Footer ====
        fluidRow(column(
            width = 12,
            align = 'center',
            footer_tag
        ),
        br(), br())
    )
))
