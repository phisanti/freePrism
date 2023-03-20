select_test_ui <- tabPanel("Chosing the right test",
  selectInput("num_groups", "How many treatment groups do you have?",
              choices = c("", "1", "2", "3", "4", "5+")),
  
  conditionalPanel(
    condition = "input.num_groups != '1'",
    radioButtons("variable_type", "Are your treatment variables categorical or numeric?",
                 selected = "",
                 choices = c("Categorical", "Numeric"))
  ),
  
  conditionalPanel(
    condition = "input.num_groups == '1' || input.variable_type == 'Categorical'",
    selectInput("test", "Choose a statistical test:",
                choices = c("", "Chi-squared test", "Fisher's exact test"))
  ),
  
  conditionalPanel(
    condition = "input.num_groups == '1' || input.variable_type == 'Numeric'",
    selectInput("normality", "Is your data normally distributed?",
                choices = c("", "Yes", "No")),
    radioButtons("paired", "Are your samples paired?",
                 choices = c("Yes", "No"))
  ),
  
  conditionalPanel(
    condition = "input.normality == 'Yes'",
    selectInput("test", "Choose a statistical test:",
                choices = c("", "t-test", "ANOVA", "Linear regression"))
  ),
  
  conditionalPanel(
    condition = "input.normality == 'No'",
    selectInput("test", "Choose a statistical test:",
                choices = c("", "Wilcox test", "Kruskal-Wallis test"))
  ),
  
  conditionalPanel(
    condition = "input.test == 'ANOVA'",
    selectInput("factor", "Which factor do you want to test?",
                choices = c("", "Treatment", "Time", "Dose")),
    checkboxInput("interaction", "Include interaction term?", value = FALSE)
  )
)