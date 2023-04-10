#' Comparison of Means Module
#'
#' @description This module performs a two-sample comparison of means analysis
#' using either a t-test or a Wilcoxon rank-sum test, depending on the user's input.
#' The input corresponds to all the arguments for the R \code{t.test} and \code{wilcox.test} functions.
#'
#' @param id A character string that specifies the namespace for the module.
#' @param d A reactive expression that returns the data to be used in the analysis.
#'
#' @return A module server function that can be called within a Shiny application.
#'
#' @examples
#' # In the UI of a Shiny app
#' comp_means_moduleUI("compMeans")
#'
#' # In the server function of a Shiny app
#' comp_means_module("compMeans", reactive({ myData }))
#' @noRd
#' @export
comp_means_module <- function(id, d) {
  moduleServer(id, function(input, output, session) {
    modns <- NS(id)
    
    # Create reactive objects
    react_two_sample <- eventReactive(input$run_analysis,{
      req(input$treatment != "" & input$treatment != input$variable)
      req(check_n_levels(d(), input$treatment) == 2)
      
      comp_means <- select_means_test(d(), input)
      comp_means
    })
    
    react_two_sample_plot <- eventReactive(input$plot_analysis,{
      req(input$treatment != "" & input$treatment != input$variable)
      comp_means_plot <- plot_one_comp_m(d(), input, react_two_sample()[[1]])
      list(comp_means_plot)
    })
    
    # Output tables and plots
    output$text <- renderUI({
      text <- react_two_sample()[[2]]
      tagList(
      h2(text[1]),
      p(text[2]),
      p(text[3]),
      h3("Summary statistic table")
      )
    })
    output$comp_means_table <- renderDT({
      req(input$treatment != "" & input$treatment != input$variable)
      react_two_sample()[[1]] %>%
        round_siginf_table
    })
    
    output$com_m_plot <- renderPlot({
      req(input$treatment != "" & input$treatment != input$variable)
      react_two_sample_plot()[[1]]
    })
    output$plot_dl <- download_plot(react_two_sample_plot()[[1]], id, input)
  })
}

#' T-test Function
#'
#' @description This function performs a t-test on the given data and returns
#' a data table with the results of the test.
#'
#' @param d A data frame or data table containing the data to be used in the test.
#' @param treatment A character string specifying the name of the column in \code{d} that contains the treatment variable.
#' @param variable A character string specifying the name of the column in \code{d} that contains the response variable.
#' @param ref.group A character string specifying the reference group for the test.
#' If \code{NULL}, the first group in alphabetical order is used as the reference group.
#' @param paired A logical value indicating whether to perform a paired t-test.
#' @param var_equal A logical value indicating whether to assume equal variances between groups.
#' @param alt_h A character string specifying the alternative hypothesis for the test.
#' Must be one of \code{"two.sided"}, \code{"less"}, or \code{"greater"}.
#'
#' @return A data table with the results of the t-test.
#'
#' @examples
#' # Load example data
#' library(data.table)
#' d <- data.table(treatment = c(rep("A", 10), rep("B", 10)),
#'                 response = c(rnorm(10, mean = 0), rnorm(10, mean = 1)))
#'
#' # Perform a t-test
#' my_t_test(d, treatment = "treatment", variable = "response")
#' @noRd
#' @export
my_t_test <- function(d,
                      treatment, 
                      variable, 
                      ref.group = NULL,
                      paired = FALSE, 
                      var_equal = TRUE, 
                      alt_h = "two.sided") {
  
  # Load vars
  
  local_d <- copy(d)
  formula_obj <- paste(variable, "~", treatment) %>%
    formula(.)
  
  # Perform T-test
  test <- t_test(formula = formula_obj, 
                     data = local_d,
                     detailed = T,
                     ref.group = ref.group,
                     paired = paired, 
                     var.equal = var_equal,
                     alternative = alt_h) 
  #  data.table
  test_out <- data.table(test)
  attr(test_out, 'args') <- attr(test, 'args')
  col_names <- colnames(test_out)
  col_remove <- col_names %in% c("n1", "n2", "alternative", "p.adj.signif")
  test_out <- test_out[, -col_names[col_remove],
                       with = FALSE]
  class(test_out) <- c("data.frame", "data.table", "t_test")
  if ("estimate1" %in% col_names) {
    setcolorder(test_out, c(".y.", "group1","estimate1", "group2","estimate2",
                            "estimate"))
  } else {
    setcolorder(test_out, c(".y.", "group1", "group2",
                            "estimate"))
  }
  return(test_out)
}

#' Wilcoxon Rank-Sum Test Function
#'
#' @description This function performs a Wilcoxon rank-sum test (also known as a Mann-Whitney U test)
#' on the given data and returns a data table with the results of the test.
#'
#' @param d A data frame or data table containing the data to be used in the test.
#' @param treatment A character string specifying the name of the column in \code{d} that contains the treatment variable.
#' @param variable A character string specifying the name of the column in \code{d} that contains the response variable.
#' @param ref.group A character string specifying the reference group for the test.
#' If \code{NULL}, the first group in alphabetical order is used as the reference group.
#' @param paired A logical value indicating whether to perform a paired Wilcoxon rank-sum test.
#' @param alt_h A character string specifying the alternative hypothesis for the test.
#' Must be one of \code{"two.sided"}, \code{"less"}, or \code{"greater"}.
#'
#' @return A data table with the results of the Wilcoxon rank-sum test.
#'
#' @examples
#' # Load example data
#' library(data.table)
#' d <- data.table(treatment = c(rep("A", 10), rep("B", 10)),
#'                 response = c(rnorm(10, mean = 0), rnorm(10, mean = 1)))
#'
#' # Perform a Wilcoxon rank-sum test
#' my_w_test(d, treatment = "treatment", variable = "response")
#'
#' @noRd
#' @export
my_w_test <- function(d,
                      treatment, 
                      variable, 
                      ref.group = NULL,
                      paired = FALSE, 
                      alt_h = "two.sided") {
  
  # Load vars
  
  local_d <- copy(d)
  formula_obj <- paste(variable, "~", treatment) %>%
    formula(.)
  paired_logical <- fcase(paired == "TRUE", TRUE,
                          paired == "FALSE", FALSE, 
                          default = FALSE)
  test <- wilcox_test(formula = formula_obj, 
                          data = local_d,
                          detailed = T,
                          ref.group = ref.group,
                          paired = paired_logical, 
                          alternative = alt_h)
  #  data.table
  test_out <- data.table(test)
  col_remove <- colnames(test_out) %in% c("n1", "n2", "conf.low", "conf.high",
                                          "alternative", "p.adj.signif")
  setcolorder(test_out, c(".y.", "group1", "group2",
                          "estimate"))
  
  attr(test_out, 'args') <- attr(test, 'args')
  test_out <- test_out[, -colnames(test_out)[col_remove],
                       with = FALSE]
  class(test_out) <- c("data.frame", "data.table", "wilcox_test")
  return(test_out)
}

#' Select Means Test Function
#'
#' @description This function performs either a t-test or a Wilcoxon rank-sum test
#' on the given data, depending on the user's input, and returns a data table 
#' with the results of the test.
#'
#' @param d A data frame or data table containing the data to be used in the test.
#' @param input A list containing the user's input for the test parameters.
#' The list should contain the following elements:
#' \itemize{
#'   \item \code{treatment}: A character string specifying the name of the column
#' in \code{d} that contains the treatment variable.
#'   \item \code{variable}: A character string specifying the name of the column 
#' in \code{d} that contains the response variable.
#'   \item \code{mean_test}: A character string specifying which test to perform. 
#' Must be one of \code{"T-test"} or \code{"Wilcox-test"}.
#'   \item \code{ref.group}: A character string specifying the reference group 
#' for the test. If \code{NULL}, the first group in alphabetical order is used 
#' as the reference group.
#'   \item \code{paired}: A logical value indicating whether to perform a paired 
#' t-test (if \code{mean_test} is \code{"T-test"}) or a paired Wilcoxon rank-sum 
#' test (if \code{mean_test} is \code{"Wilcox-test"}).
#'   \item \code{var_equal}: A logical value indicating whether to assume equal 
#' variances between groups (only used if \code{mean_test} is \code{"T-test"}).
#'   \item \code{alt_h}: A character string specifying the alternative hypothesis 
#' for the test. Must be one of \code{"two.sided"}, \code{"less"}, or \code{"greater"}.
#' }
#'
#' @return A data table with the results of the selected means test.
#'
#' @examples
#' # Load example data
#' library(data.table)
#' d <- data.table(treatment = c(rep("A", 10), rep("B", 10)),
#'                 response = c(rnorm(10, mean = 0), rnorm(10, mean = 1)))
#'
#' # Perform a t-test
#' input <- list(treatment = "treatment", variable = "response", mean_test = "T-test")
#' select_means_test(d, input)
#'
#' # Perform a Wilcoxon rank-sum test
#' input <- list(treatment = "treatment", variable = "response", mean_test = "Wilcox-test")
#' select_means_test(d, input)
#'
#' @noRd
#' @export

select_means_test <- function(d, input){

  # Load variables
  treatment <- input$treatment
  variable <- input$variable
  paired <- input$paired
  ref_group <- input$ref_group
  alt_h <- input$alt_h
  
  if (ref_group == "none") {
    ref.group <- NULL
  }
  # Perform chosen test
  if (input$mean_test == "T-test") {
    var_equal <- input$var_equal == TRUE
    test_out <- my_t_test(d,
                          treatment, 
                          variable, 
                          ref.group = ref_group,
                          paired = paired, 
                          var_equal = var_equal, 
                          alt_h = alt_h)
  } else if (input$mean_test == "Wilcox-test") {
    test_out <- my_w_test(d,
                          treatment, 
                          variable, 
                          ref.group = ref.group,
                          paired = paired, 
                          alt_h = alt_h)
  } else {
    test_out <- data.table(Issue = "Selected test not found")
  }
  
  test_report <- reportr(test_out)
  return(list(test_out, test_report))
}

