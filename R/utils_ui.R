#' @title UI Utils 
#'
#' @description These are list with different sections of the UI
#' @return UI shiny elements.
#'
#' @noRd
#' @export
colpalettes <- c("aaas", "d3", "flauti", "frontiers", "futurama",
                 "gsea", "igv", "jama", "jco", "lancet", "material",
                 "nejm", "npg", "uchicago", "ucscgb")

#' @title UI Utils 
#'
#' @description These are list with different sections of the UI
#' @return UI shiny elements.
#'
#' @noRd
#' @export

colinput <- function(width, id, label, choices, ...) {
  shiny::column(width,
                shiny::selectInput(id, 
                                   label,
                                   choices = choices, ...))
  
}
#' @title UI Utils 
#'
#' @description These are list with different sections of the UI
#' @return UI shiny elements.
#'
#' @noRd
#' @export

common_input <- function(panel, modns) {
  if (panel == "stats") {
    common_input <- tagList(   
      shiny::selectInput(modns("treatment"), 
                         "treatment",
                         choices = ""),
    shiny::selectInput(modns("variable"), 
                       "Select target variable", 
                       choices = ""),
    shiny::fluidRow(
      shiny::column(5,
                    shiny::actionButton(modns("run_analysis"), 
                                        "Run Analysis")))
    )
  } else if (panel == "plotting") {
    common_input <- shiny::fluidRow(shiny::column(5,
    shiny::actionButton(modns("plot_analysis"), 
                        "Plot Analysis")))
  }
  return(common_input)
  
}


#' @title UI Utils 
#'
#' @description These are list with different sections of the UI
#' @return UI shiny elements.
#'
#' @noRd
#' @export
explore_ui <- function(id, panel) {
  modns <- NS(id)
  if (panel == "stats") {
    sidebar <- common_input(panel, modns)
  } else if (panel == "plotting") {
    sidebar <- tagList(shiny::fluidRow(
      colinput(5, modns("plot_type"), "Select a Plot type:",
               c("histogram", "QQ", "Correation")),
      colinput(5, modns("colpal"), "Select colour palette:",
               colpalettes),
    ),
    common_input(panel, modns)
    ) 
  }
  return(sidebar)
}
#' @title UI Utils 
#'
#' @description These are list with different sections of the UI
#' @return UI shiny elements.
#'
#' @noRd
#' @export
compmeans_ui <- function(id, panel) {
  
  modns <- NS(id)
  
  if (panel == "stats") {
    sidebar <- tagList(shiny::fluidRow(
      colinput(3, modns("ref_group"), "Select a control group:", 
               choices = NULL),
      colinput(3, modns("var_equal"), "Are variances equal:", 
               choices = c(TRUE, FALSE), 
               selected = TRUE),
      colinput(3, modns("paired"), "Is the test paired:",
               choices = c(TRUE, FALSE),
               selected = FALSE)
    ),
    fluidRow(
      colinput(3, modns("mean_test"), "Select a test:", 
               choices = c("T-test",
                           "Wilcox-test")),
      colinput(3, modns("alt_h"), 
               "Select a test:", 
               choices = c("two.sided", "less", "greater"))),
    common_input(panel, modns)
    
    )
  } else if (panel == "plotting") {
    sidebar <- tagList(shiny::fluidRow(
      colinput(5, modns("plot_type"), "Select a Plot type:",
               c("barplot", "boxplot", "histogram")),
      colinput(5, modns("colpal"), "Select colour palette:",
               colpalettes)
    ), 
    common_input(panel, modns)
    )
  }
  
  return(sidebar)
}

#' @title UI Utils 
#'
#' @description These are list with different sections of the UI
#' @return UI shiny elements.
#'
#' @noRd
#' @export
oneway_ui <- function(id, panel) {
  
  modns <- NS(id)
  if (panel == "stats") {
    sidebar <- tagList(shiny::fluidRow(
      colinput(5, modns("ref_group"), "Select a control group:", 
               choices = ""),
      colinput(7, modns("one_test"), "Select a test:",
               choices = c("One-Way-ANOVA",
                           "Kruskall-Wallis"))),
      shiny::fluidRow(
        colinput(5, modns("anova_type"), "Select a ANOVA type:",
                 choices = c("I", "II","III")),
        colinput(7, modns("p_adjust_method"), 
                 "Select method to adjust p-value:",
                 choices = p.adjust.methods)),
      common_input(panel, modns))
    
  } else if (panel == "plotting") {
    sidebar <- tagList(shiny::fluidRow(
      colinput(5, modns("plot_type"), "Select a Plot type:",
               c("barplot", "boxplot")),
      colinput(5, modns("colpal"), "Select colour palette:",
               colpalettes),
      ),
      common_input(panel, modns)
    )
  }
  
  return(sidebar)
}
#' @title UI Utils 
#'
#' @description These are list with different sections of the UI
#' @return UI shiny elements.
#'
#' @noRd
#' @export
twoway_ui <- function(id, panel) {
  
  modns <- NS(id)
  if (panel == "stats") {
    sidebar <- shiny::tagList(shiny::fluidRow(
      colinput(5, modns("ref_group"), "Select a control group:", 
               choices = NULL),
      colinput(7, modns("is_normal"), 
               "Is your data normally distribute:",
               choices = c(TRUE, FALSE)),
    ),
    shiny::fluidRow(
      colinput(5, modns("anova_type"), "Select a ANOVA type:", 
               choices = c("I", "II","III")),
      colinput(7, "p_adjust_method", "Select method to adjust p-value:",
               choices = p.adjust.methods)
    ),
    shiny::fluidRow(
      colinput(5, modns("allow_interaction"), "Allow inteaction in your model:",
               choices = c(FALSE, TRUE))),
    common_input(panel, modns)
    )
  } else if (panel == "plotting") {
    sidebar <- tagList(shiny::fluidRow(
      colinput(6, modns("plot_type"), "Select a Plot type:",
               c("barplot", "boxplot")),
      colinput(6, modns("xvar"), "Select the X variable:",
               c("")),
      colinput(6, modns("yvar"), "Select the Y variable:",
               c("")),
      colinput(6, modns("colvar"), "Select the colouring variable:",
               c("")),
      colinput(6, modns("colpal"), "Select colour palette:",
               colpalettes)
    ),
    common_input(panel, modns))
  }
  
  return(sidebar)
}

linreg_ui <- function(id, panel) {
  modns <- NS(id)
  if (panel == "stats") {
    
      sidebar <- tagList(shiny::fluidRow(column(3,numericInput(modns("cilevel"), 
                            label = "Select Conf. Interval: ", 
                            min = 0.01, 
                            max = .99, 
                            value = .95)
      )),
      common_input(panel, modns))
  } else if (panel == "plotting") {
    sidebar <- tagList(shiny::fluidRow(
      colinput(5, modns("plot_type"), "Select a Plot type:",
               c("placeholder")),
      colinput(5, modns("xvar"), "Select the X variable:",
               c("")),
      colinput(5, modns("yvar"), "Select the Y variable:",
               c("")),
      colinput(5, modns("colvar"), "Select the colouring variable:",
               c("")),
      colinput(5, modns("colpal"), "Select colour palette:",
               colpalettes)
    ),
    common_input(panel, modns))
  }
  
  return(sidebar)
}
#' @title UI Utils 
#'
#' @description These are list with different sections of the UI
#' @return UI shiny elements.
#'
#' @noRd
#' @export
sidebar_elements_ui <- list(
  
  # Two means comparison
  compmean = tagList(shiny::fluidRow(
    colinput(3,"ref_group", "Select a control group:", 
             choices = NULL),
    colinput(3,"var_equal", "Are variances equal:", 
             choices = c(TRUE, FALSE), 
             selected = TRUE),
    colinput(3,"paired", "Is the test paired:",
             choices = c(TRUE, FALSE),
             selected = FALSE)
    ),
  fluidRow(
    colinput(3, "mean_test", "Select a test:", 
                       choices = c("T-test",
                                   "Wilcox-test")),
    colinput(3, "alt_h", 
                       "Select a test:", 
                       choices = c("two.sided", "less", "greater")))
  ),
  
  # One-Way ANOVA
  oneway = tagList(shiny::fluidRow(
    colinput(5, "ref_group", "Select a control group:", 
                                     choices = ""),
    colinput(7, "one_test", "Select a test:",
                       choices = c("One-Way-ANOVA",
                                   "Kruskall-Wallis"))),
  shiny::fluidRow(
    colinput(5, "anova_type", "Select a ANOVA type:",
                       choices = c("I", "II","III")),
    colinput(7,"p_adjust_method", 
                                     "Select method to adjust p-value:",
                                     choices = p.adjust.methods))),
  # Two-way ANOVA
  twoway = shiny::tagList(shiny::fluidRow(
    colinput(5,"ref_group", "Select a control group:", 
                                     choices = NULL),
    colinput(7,"is_normal", 
                       "Is your data normally distribute:",
                       choices = c(TRUE, FALSE)),
  ),
    shiny::fluidRow(
      colinput(5, "anova_type", "Select a ANOVA type:", 
               choices = c("I", "II","III")),
      colinput(7, "p_adjust_method", "Select method to adjust p-value:",
               choices = p.adjust.methods)
      ),
  shiny::fluidRow(
    colinput(5, "allow_interaction", "Allow inteaction in your model:",
             choices = c(FALSE, TRUE))
  )),
  # Linear Regression
  linreg = shiny::fluidRow(
    column(3,numericInput(inputId = "cilevel", 
                          label = "Select Conf. Interval: ", 
                          min = 0.01, 
                          max = .99, 
                          value = .95)
    )
  ),
  
  ### Plotting tools ###
  plot_tools = list(
    # Exploratory analysis tools
    exploratory = shiny::fluidRow(
      colinput(5, "plot_type", "Select a Plot type:",
               c("histogram", "QQ", "Correation")),
      colinput(5, "colpal", "Select colour palette:",
               colpalettes),
    ),
    # Mean comparison tools
    meancomp = shiny::fluidRow(
      colinput(5, "plot_type", "Select a Plot type:",
               c("barplot", "boxplot", "histogram")),
      colinput(5, "colpal", "Select colour palette:",
               colpalettes)
    ),
    # One-way ANOVA
    oneway = shiny::fluidRow(
      colinput(5, "plot_type", "Select a Plot type:",
               c("barplot", "boxplot")),
      colinput(5, "colpal", "Select colour palette:",
               colpalettes)
    ),
    twoway = shiny::fluidRow(
      colinput(5, "plot_type", "Select a Plot type:",
               c("barplot", "boxplot")),
      colinput(5, "xvar", "Select the X variable:",
               c("")),
      colinput(5, "yvar", "Select the Y variable:",
               c("")),
      colinput(5, "colvar", "Select the colouring variable:",
               c("")),
      colinput(5, "colpal", "Select colour palette:",
               colpalettes)
    ),
    linreg = shiny::fluidRow(
      colinput(5, "plot_type", "Select a Plot type:",
               c("placeholder")),
      colinput(5, "xvar", "Select the X variable:",
               c("")),
      colinput(5, "yvar", "Select the Y variable:",
               c("")),
      colinput(5, "colvar", "Select the colouring variable:",
               c("")),
      colinput(5, "colpal", "Select colour palette:",
               colpalettes)
    )
  )
)

#' @title UI text 
#'
#' @description These are blocks of text for the UI. The reasoning to place them
#' here is to release space from the main app_ui functionality.
#' @return UI shiny elements.
#'
#' @noRd
#' @export
mainpanel_txt_ui <- 
  list(compare_means = 
         list(
           intro = c(" This tab can perform two sample tests including t-test, paired t-test, welch-test, and wilcox test. If you don't know which test use, please refer tot the Chosing the right test tab",
                     "To use the app, start by selecting the treatment variable and the target variable from the controllers provided on the sidebar. Next, choose the control group if applicable. Then, select whether the variances of the groups are equal or not, whether the test is paired or not, and the type of alternative hypothesis (alternative, lower, greater).",
                     "Once you have made your selections, click on the 'Run Analysis' button to perform the selected test. The results will be displayed in the output panel, which will show the calculated p-value, the test statistic, and the confidence interval for the difference between the means of the groups. This will generate a table with the appropiate summary statistics and then allow you generate a plot to visualise the data")
                           ),
       one_anova = list(
         intro = c("To use the multiple group comparison tool, start by selecting a treatment and target variable. If applicable, select a control group from the dropdown menu provided. This control group will serve as a reference for comparison to the other groups. Next, select the type of test to perform - either the one-way ANOVA or the Kruskal-Wallis test.",
                   "After selecting the test, choose the type of ANOVA to perform from the dropdown menu. There are three types of ANOVA - Type I, Type II, and Type III - which differ in their method of calculating sums of squares. Finally, select the method to adjust p-values from the available choices provided.",
                   "Once all selections have been made, click on the 'Run Analysis' button to perform the selected test. The results will be displayed in the output panel, which will show the calculated p-value, the test statistic, and the degrees of freedom for the selected test.",
                   "In addition to performing the ANOVA or Kruskal-Wallis test, the one-way ANOVA tool also includes a post-hoc test to determine which specific group differences are statistically significant. If the data is normally distributed, the tool uses the Tukey-HSD test, which calculates the minimum significant difference between groups. If the data is not normally distributed, the Dunn test is used, which is a non-parametric test that compares all pairs of groups using the Holm correction method to adjust for multiple comparisons. The post-hoc test results will also be displayed in the output panel, providing users with a more comprehensive understanding of the differences between the groups. It is important to note that post-hoc tests should only be conducted if the overall ANOVA or Kruskal-Wallis test shows significant differences between the groups."
           
         )
       ),
       two_anova = list(
         intro = c("This tab can perform Two-way-ANOVA test. It runs a two-way ANOVA test on normally distributed data, and ANOVA on the ranked data if the data is not normally distributed.", 
                   "To use the two-way ANOVA test, start by selecting a the treatments and target variable. If applicable, select a control group from the dropdown menu provided. Next, indicate whether the data is normally distributed or not by selecting the appropriate option. If the data is not normally distributed, the tool will perform a non-parametric test instead of an ANOVA test.",
                   "After selecting the appropriate data type, choose the type of ANOVA to perform from the dropdown menu. There are three types of ANOVA - Type I, Type II, and Type III - which differ in their method of calculating sums of squares. Finally, select the method to adjust p-values from the available choices provided.",
                   "the two-way ANOVA test also allows users to include an interaction term in their model. An interaction term is used to determine if the effect of one independent variable on the dependent variable is affected by the level of the other independent variable. If there is a significant interaction effect, it suggests that the relationship between the independent variables and the dependent variable is more complex than a simple additive effect."
                   )
       ),
       lin_reg = list(
         intro = c("This tab can perform linear regression. Regression analysis is a statistical technique used to model the relationship between a dependent variable (also known as the response variable) and one or more independent variables (also known as predictor variables). The linear regression tool provides a user-friendly interface to perform linear regression analysis, which assumes a linear relationship between the variables.",
                   "The treatment variable represents the independent variable, while the target variable represents the dependent variable. Additionally, users can select the level of confidence interval they want to use in the analysis by adjusting the 'Select Conf. Interval' slider.",
                   "Once the analysis is run, the tool provides a summary of the regression results, including the coefficients and p-values for each variable, as well as the R-squared value, which represents the goodness of fit of the model. The tool also provides various plots, such as a scatter plot of the data with the regression line overlaid, a residual plot to check the assumption of constant variance, and a normal probability plot to check the assumption of normality."
                   
                   )
       )
  
)