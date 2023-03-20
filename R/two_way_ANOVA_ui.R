two_way_ANOVA_ui <- fluidRow(
  column(5,
         selectInput("is_nonparametric", 
                     "Is your data normally distribute:",
                     choices = c(TRUE,
                                 FALSE))),
  column(5,
         selectInput("anova_type", 
                     "Select a ANOVA type:",
                     choices = c("I", "II","III"))),
)