one_way_ANOVA_ui <- fluidRow(
  column(5,
         selectInput("one_test", 
                     "Select a test:",
                     choices = c("One-Way-ANOVA",
                                 "Kruskall-Wallis"))),
  column(5,
         selectInput("anova_type", 
                     "Select a ANOVA type:",
                     choices = c("I", "II","III"))),
  )