# This script contains the funtions required for the compare means panel
Compare_means_ui <- fluidRow(
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
                     choices = c("two.sided", "less", "greater"))))
  