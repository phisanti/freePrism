#' Color palettes
#'
#' This is a character vector containing the names of several color palettes.
#'
#' @return A character vector of color palette names.
colpalettes <- c("aaas", "d3", "flauti", "frontiers", "futurama",
                 "gsea", "igv", "jama", "jco", "lancet", "material",
                 "nejm", "npg", "uchicago", "ucscgb")

#' Column input
#'
#' This function creates a column containing a select input in a shiny app.
#'
#' @param width An integer specifying the width of the column.
#' @param id A string specifying the ID module.
#' @param label A string specifying the label for the select input.
#' @param choices A vector of strings specifying the choices of the select input.
#' @param ... Additional arguments passed to shiny::selectInput().
#'
#' @return A column containing a select input.
#'
#' @import shiny

colinput <- function(width, id, label, choices, ...) {
  shiny::column(width,
                shiny::selectInput(id, 
                                   label,
                                   choices = choices, ...))
  
}
#' Common input
#'
#' This function creates the common inputs for each tab. It provides statistical
#' as well as plotting commands.
#'
#' @param panel A string indicating whether the panel is a "stats" panel or a 
#' "plotting" panel.
#' @param modns A string to namespace inputs to avoid id conflicts when using 
#' multiple instances of the same module.
#'
#' @return A tag list containing a select input for the treatment and a select 
#' input for the target variable if the panel is "stats". Otherwise, it returns 
#' a numeric input for the download image width and a numeric input for the 
#' download image height if the panel is "plotting".
#'
#' @import shiny

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
    common_input <- tagList(
      shiny::fluidRow(
        shiny::column(6,
                    shiny::numericInput(modns("dl_width"), value = 16,
                                        "Download image width (cm)")),
      shiny::column(6,
                    shiny::numericInput(modns("dl_height"), value = 16, 
                                        "Download image height (cm)"))),
      shiny::fluidRow(shiny::column(5,
    shiny::actionButton(modns("plot_analysis"), 
                        "Plot Analysis"))
    ))
  }
  return(common_input)
  
}

#' Explore UI
#'
#' This function generates the user interface for the Explore tab of a Shiny application.
#'
#' @param id A character string specifying the namespace for the UI elements.
#' @param panel A character string specifying the panel to display in the UI ("stats" or "plotting").
#'
#' @return A Shiny UI object.
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
#' Mean comparison UI
#'
#' This function generates the user interface for the Explore tab of a Shiny application.
#'
#' @param id A character string specifying the namespace for the UI elements.
#' @param panel A character string specifying the panel to display in the UI ("stats" or "plotting").
#'
#' @return A Shiny UI object.
compmeans_ui <- function(id, panel) {
  
  modns <- NS(id)
  
  if (panel == "stats") {
    sidebar <- tagList(shiny::fluidRow(
      colinput(4, modns("ref_group"), "Select a control group:", 
               choices = NULL),
      colinput(4, modns("var_equal"), "Are variances equal:", 
               choices = c(TRUE, FALSE), 
               selected = TRUE),
      colinput(4, modns("paired"), "Is the test paired:",
               choices = c(TRUE, FALSE),
               selected = FALSE)
    ),
    fluidRow(
      colinput(4, modns("mean_test"), "Select a test:", 
               choices = c("T-test",
                           "Wilcox-test")),
      colinput(4, modns("alt_h"), 
               "Select a test:", 
               choices = c("two.sided", "less", "greater"))),
    common_input(panel, modns)
    
    )
  } else if (panel == "plotting") {
    sidebar <- tagList(shiny::fluidRow(
      colinput(5, modns("plot_type"), "Select a Plot type:",
               c("barplot", "boxplot", "histogram")),
      colinput(5, modns("colpal"), "Select colour palette:",
               colpalettes),
      colinput(5, modns("pval_format"), "Display p-val as:",
               c("star", "numeric"))
    ), 
    common_input(panel, modns)
    )
  }
  
  return(sidebar)
}

#' ANOVA One-way UI
#'
#' This function generates the user interface for the ANOVA One-way tab of a Shiny application.
#'
#' @param id A character string specifying the namespace for the UI elements.
#' @param panel A character string specifying the panel to display in the UI ("stats" or "plotting").
#'
#' @return A Shiny UI object.
oneway_ui <- function(id, panel) {
  
  modns <- NS(id)
  if (panel == "stats") {
    sidebar <- tagList(shiny::fluidRow(
      colinput(5, modns("ref_group"), "Select a control group:", 
               choices = ""),
      colinput(7, modns("test"), "Select a test:",
               choices = c("One-Way-ANOVA",
                           "Kruskall-Wallis"))),
      shiny::fluidRow(
        colinput(5, modns("anova_type"), "Select a ANOVA type:",
                 choices = c("I", "II","III")),
        colinput(7, modns("p_adjust_method"), 
                 "Select method to adjust p-value:",
                 choices = p.adjust.methods)),
      shiny::fluidRow(
        colinput(12, modns("posthoc"),
                 "Select a Post-Hoc Test", 
                  selected = "Tukey HSD", 
                  choices = c("Tukey HSD", "Mult. T-test")
                 )
        ),
      common_input(panel, modns)
      )
    
  } else if (panel == "plotting") {
    sidebar <- tagList(shiny::fluidRow(
      colinput(5, modns("plot_type"), "Select a Plot type:",
               c("barplot", "boxplot")),
      colinput(5, modns("colpal"), "Select colour palette:",
               colpalettes),
      colinput(5, modns("pval_format"), "Display p-val as:",
               c("star", "numeric"))
      ),
      common_input(panel, modns)
    )
  }
  
  return(sidebar)
}

#' ANOVA Two-way UI
#'
#' This function generates the user interface for the ANOVA Two-way tab of a Shiny application.
#'
#' @param id A character string specifying the namespace for the UI elements.
#' @param panel A character string specifying the panel to display in the UI ("stats" or "plotting").
#'
#' @return A Shiny UI object.
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
               colpalettes),
      colinput(6, modns("betweet_type"), "Stat. comparison:",
               c("within", "between")),
      colinput(6, modns("pval_format"), "Display p-val as:",
               c("star", "numeric"))
      
    ),
    common_input(panel, modns))
  }
  
  return(sidebar)
}

linreg_ui <- function(id, panel) {
  modns <- NS(id)
  if (panel == "stats") {
    
      sidebar <- tagList(
        shiny::fluidRow(column(3,numericInput(modns("cilevel"), 
                            label = "Select Conf. Interval: ", 
                            min = 0.01, max = .99, value = .95)),
                        colinput(5, modns("allow_interaction"), 
                                 "Allow inteaction in your model:",
                                 choices = c(FALSE, TRUE))),
      common_input(panel, modns))
  } else if (panel == "plotting") {
    sidebar <- tagList(shiny::fluidRow(
      colinput(6, modns("plot_type"), "Select a Plot type:",
               c("model check", "scatter")),
      colinput(6, modns("xvar"), "Select the X variable:",
               c(""))),
      shiny::fluidRow(
      colinput(6, modns("yvar"), "Select the Y variable:",
               c("")),
      colinput(6, modns("colvar"), "Select the colouring variable:",
               c(""))),
      shiny::fluidRow(
      colinput(6, modns("colpal"), "Select colour palette:",
               colpalettes)
    ),
    common_input(panel, modns))
  }
  
  return(sidebar)
}

#' Mainpanel Text User Interface
#'
#' This function contains the text for the four types of analysis tools in the 
#' shiny app.
#' 
#' @return A character vector with the text for each section.
#' @export
mainpanel_txt_ui <- 
  list(
    howto = 
      list(
        intro = c("Welcome to freePrism! This platform allows you to perform data exploration, two sample comparison, One and Two-way-ANOVA, and regression analysis.",
                  "To begin, you will need to upload your own data using the side menu. Once your data is uploaded, you can then choose which statistical test you would like to perform by selecting one of the tabs available.",
                  "Each tab corresponds to a different statistical test, and you can input your desired commands and arguments for each test in the side menu. Once you have entered your inputs, click the 'Run Analysis' button to execute the test.",
                  "After running the test, the results will be displayed on the main page along with your data. Additionally, you have the option to plot the data and if you wish you can also download the generated plots in PDF format for further editing.",
                  "Thank you for using freePrism!")
      ),
    explore = 
      list(
        intro = c("The module is used to explore a dataset and generate visualizations based on the input parameters. Here you can select a target variable and a grouping variable if apply and then the tool will generate a table with the number of samples, mean, median, variance and inter quantile range. Also, the tool also runs a Shapiro wilks test to verify in the data follows a normal distribution",
                  "The module also includes some data visualization tools such as a QQ plot, a histogram, and a correlation plot, which can be colored by a variable.",
                  "To use the app, first select a target variable. Then, if applicable select a grouping variable (treatment). Next click on 'Run Analysis' to generate the tables. For the graphic data, select the trype of plot and the colour palette. Then, click on 'Plot Analysis'")
      ),
    compare_means = 
         list(
           intro = c("This tab can perform two-sample tests, including a t-test, paired t-test, welch-test, and Wilcox test.",
                     "To use the app, select the treatment variable and the target variable from the controllers provided on the sidebar. Next, choose the control group if applicable. Then, select whether the variances of the groups are equal or not, whether the test is paired or not, and the type of alternative hypothesis (alternative, lower, greater).",
                     "Once you have made your selections, click on the 'Run Analysis' button to perform the selected test. The results will be displayed in the output panel, which will show the calculated p-value, the test statistic, and the confidence interval for the difference between the means of the groups. This will generate a table with the appropiate summary statistics and then allow you generate a plot to visualise the data")
                           ),
       one_anova = list(
         intro = c("To use the multiple-group comparison tool, start by selecting a treatment and target variable. If applicable, select a control group from the dropdown menu provided. This control group will serve as a reference for comparison to the other groups. Next, select the type of test to perform - either the one-way ANOVA or the Kruskal-Wallis test.",
                   "After selecting the test, choose the type of ANOVA to perform from the dropdown menu. There are three types of ANOVA - Type I, Type II, and Type III - which differ in their method of calculating sums of squares. Finally, select the method to adjust p-values from the available choices provided.",
                   "Once all selections have been made, click on the 'Run Analysis' button to perform the selected test. The results will be displayed in the output panel, which will show the calculated p-value, the test statistic, and the degrees of freedom for the selected test.",
                   "In addition to performing the ANOVA or Kruskal-Wallis test, the one-way ANOVA tool also includes a post-hoc test to determine which specific group differences are statistically significant. If the data is normally distributed, the tool uses the Tukey-HSD test, which calculates the minimum significant difference between groups. If the data is not normally distributed, the Dunn test is used, which is a non-parametric test that compares all pairs of groups using the Holm correction method to adjust for multiple comparisons. The post-hoc test results will also be displayed in the output panel, providing users with a more comprehensive understanding of the differences between the groups. It is important to note that post-hoc tests should only be conducted if the overall ANOVA or Kruskal-Wallis test shows significant differences between the groups."
           
         )
       ),
       two_anova = list(
         intro = c("This tab can perform a Two-way-ANOVA test. It runs a two-way ANOVA test on normally distributed data and ANOVA on the ranked data if the data is not normally distributed.", 
                   "To use the two-way ANOVA test, start by selecting the treatments and target variable. If applicable, select a control group from the dropdown menu provided. Next, indicate whether the data is normally distributed or not by selecting the appropriate option. If the data is not normally distributed, the tool will perform a non-parametric test instead of an ANOVA test.",
                   "After selecting the appropriate data type, choose the type of ANOVA to perform from the dropdown menu. There are three types of ANOVA - Type I, Type II, and Type III - which differ in their method of calculating sums of squares. Finally, select the method to adjust p-values from the available choices provided.",
                   "The two-way ANOVA test also allows users to include an interaction term in their model. An interaction term is used to determine if the effect of one independent variable on the dependent variable is affected by the level of the other independent variable. If there is a significant interaction effect, it suggests that the relationship between the independent variables and the dependent variable is more complex than a simple additive effect."
                   )
       ),
       lin_reg = list(
         intro = c("This tab can perform linear regression. Regression analysis is a statistical technique used to model the relationship between a dependent variable (also known as the response variable) and one or more independent variables (also known as predictor variables). The linear regression tool provides a user-friendly interface to perform linear regression analysis, which assumes a linear relationship between the variables.",
                   "The treatment variable represents the independent variable, while the target variable represents the dependent variable. Additionally, users can select the level of confidence interval they want to use in the analysis by adjusting the 'Select Conf. Interval' slider.",
                   "Once the analysis is run, the tool provides a summary of the regression results, including the coefficients and p-values for each variable, as well as the R-squared value, which represents the goodness of fit of the model. The tool also provides various plots, such as a scatter plot of the data with the regression line overlaid, a residual plot to check the assumption of constant variance, and a normal probability plot to check the assumption of normality."
                   
                   )
       )
  
)