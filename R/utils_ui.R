#' @title UI Utils 
#'
#' @description These are list with different sections of the UI
#'
#' @return UI shiny elements.
#'
#' @noRd
#' @export
sidebar_elements_ui <- list(
  
  compare_means_ui = fluidRow(
    column(3,
           selectInput("ref_group", "Select a control group:", 
                       choices = NULL)),
    column(3,
           selectInput("var_equal", selected = TRUE,
                       "Are variances equal:", 
                       choices = c(TRUE, FALSE))),
    column(3,
           selectInput("paired", selected = FALSE,
                       "Is the test paired:", 
                       choices = c(TRUE, FALSE))),
    column(3,
           selectInput("mean_test", 
                       "Select a test:", 
                       choices = c("T-test",
                                   "Wilcox-test"))),
    column(3,
           selectInput("alt_h", 
                       "Select a test:", 
                       choices = c("two.sided", "less", "greater")))),
  
  one_way_ANOVA_ui = fluidRow(
    column(5,
           selectInput("one_test", 
                       "Select a test:",
                       choices = c("One-Way-ANOVA",
                                   "Kruskall-Wallis"))),
    column(5,
           selectInput("anova_type", 
                       "Select a ANOVA type:",
                       choices = c("I", "II","III"))),
  ),
  
  two_way_ANOVA_ui = fluidRow(
    column(5,
           selectInput("is_nonparametric", 
                       "Is your data normally distribute:",
                       choices = c(TRUE,
                                   FALSE))),
    column(5,
           selectInput("anova_type", 
                       "Select a ANOVA type:",
                       choices = c("I", "II","III"))),
  ),
  
  lin_reg_ui = fluidRow(
    column(3,numericInput(inputId = "cilevel", 
                          label = "Select Conf. Interval: ", 
                          min = 0.01, 
                          max = .99, 
                          value = .95)
    )
  )
  
  
)