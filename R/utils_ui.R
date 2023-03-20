#' @title UI Utils 
#'
#' @description These are list with different sections of the UI
#' @return UI shiny elements.
#'
#' @noRd
#' @export
sidebar_elements_ui <- list(
  
  comp_means_ui = shiny::fluidRow(
    shiny::column(3,
                  shiny::selectInput("ref_group", "Select a control group:", 
                       choices = NULL)),
    shiny::column(3,
                  shiny::selectInput("var_equal", selected = TRUE,
                       "Are variances equal:", 
                       choices = c(TRUE, FALSE))),
    shiny::column(3,
           shiny::selectInput("paired", selected = FALSE,
                       "Is the test paired:", 
                       choices = c(TRUE, FALSE))),
    shiny::column(3,
           shiny::selectInput("mean_test", 
                       "Select a test:", 
                       choices = c("T-test",
                                   "Wilcox-test"))),
    shiny::column(3,
           shiny::selectInput("alt_h", 
                       "Select a test:", 
                       choices = c("two.sided", "less", "greater")))),
  
  one_way_ANOVA_ui = shiny::fluidRow(
    shiny::column(5,
           shiny::selectInput("one_test", 
                       "Select a test:",
                       choices = c("One-Way-ANOVA",
                                   "Kruskall-Wallis"))),
    shiny::column(5,
           shiny::selectInput("anova_type", 
                       "Select a ANOVA type:",
                       choices = c("I", "II","III"))),
  ),
  
  two_way_ANOVA_ui = shiny::fluidRow(
    shiny::column(5,
           shiny::selectInput("is_nonparametric", 
                       "Is your data normally distribute:",
                       choices = c(TRUE,
                                   FALSE))),
    shiny::column(5,
           shiny::selectInput("anova_type", 
                       "Select a ANOVA type:",
                       choices = c("I", "II","III"))),
  ),
  
  lin_reg_ui = shiny::fluidRow(
    column(3,numericInput(inputId = "cilevel", 
                          label = "Select Conf. Interval: ", 
                          min = 0.01, 
                          max = .99, 
                          value = .95)
    )
  )
)