## Globals for Shiny app
library(here)
source(here("code", "utils.R"))

## Textual constants ----
paper_title <- HTML(
    "Disproportionate opioid-related deaths among
                    non-Hispanic Black Americans in Washington DC, 1999-2018"
)
footer_tag  <- HTML(
    "Created in <a href='https://shiny.rstudio.com/'>Shiny</a> by
    <a href='https://mathewkiang.com'>Mathew Kiang</a>.
    Source code is available on
    <a href='https://github.com/mkiang/opioid_inequities'>this paper's
    Github repository</a>."
)

## More info
more_info1 <- list(
    h4("More information"),
    HTML(
        "For more information, please see our article,",
        "available at (INSERT LINK) or the",
        "<a href='https://github.com/mkiang/opioid_inequities'> associated Github repository</a>,",
        "which contains all reproducible code. To the extent allowed",
        "by the NCHS Data Use Agreement, we provide all reproducible data.",
        "Specifically, in cases when an observation is based on",
        "fewer than 10 deaths, it is suppressed from the plots and table output.",
        "When a model has only one suppressed value, the mean squared error",
        "(MSE) and sum of squared error (SSE) are suppressed."
    )
)

## Paper blurb
paper_blurb1 <-
    HTML(
        'We model the state-specific opioid-related mortality rate for
        non-Hispanic white ("White") and non-Hispanic Black ("Black")
        Americans from 1999 to 2018 using joinpoint regression models.
        We limit our analysis to 33 states with at least 100,000 white
        and Black residents and Washington DC. Joinpoint models summarize a trend by fitting
        the minimum number of line segments that provide adequate fit as
        evaluated by a permutation test. <p> While our paper focuses on DC,
        this companion application allows users to select specific areas.
        Below we plot the state-specific fitted rates for both white and
        Black residents of that state. In addition, we provide tables with
        model estimates and fit statistics.'
    )

## Biographical tags
xxxx_tag <- tags$li(a(href = "XXXX", "XXXX X XXXX"),
                    HTML(paste0("(",
                                a(href = "XXXX",
                                  "@XXXX"),
                                ")")))
mvk_tag <-
    tags$li(a(href = "https://mathewkiang.com", "Mathew V Kiang"),
            HTML(paste0(
                "(",
                a(href = "https://twitter.com/mathewkiang",
                  "@mathewkiang"),
                ")"
            )))
dr_tag <-
    tags$li(a(href = "https://profiles.stanford.edu/david-rehkopf", "David H Rehkopf"),
            HTML(paste0(
                "(",
                a(href = "https://twitter.com/drehkopf",
                  "@drehkopf"),
                ")"
            )))
act_tag <-
    tags$li(a(href = "https://connects.catalyst.harvard.edu/Profiles/display/Person/90553", "Alexander C Tsai"),
            HTML(paste0(
                "(",
                a(href = "https://twitter.com/drdrtsai",
                  "@drdrtsai"),
                ")"
            )))
sb_tag <-
    tags$li(a(href = "https://sites.google.com/stanford.edu/basulab/home",
              "Sanjay Basu"),
            HTML(paste0(
                "(",
                a(href = "https://twitter.com/sanjaybmdphd",
                  "@sanjaybmdphd"),
                ")"
            )))
mja_tag <- tags$li(a(href = "http://monicaalexander.com",
                     "Monica J Alexander"),
                   HTML(paste0(
                       "(",
                       a(href = "https://twitter.com/monjalexander",
                         "@monjalexander"),
                       ")"
                   )))

## Make a state name:abbrev dictionary ----
st_subset <- return_st_info(subset_states = TRUE)
st_names <- setNames(st_subset %>% pull(abbrev) %>% as.list(.),
                     st_subset %>% pull(name))
