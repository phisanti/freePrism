#' Linear Regression module for Shiny app
#'
#' This module creates a user interface and a server for a linear regression test
#' and related plots. It also updates its inputs with the corresponding information
#' from the data file.
#'
#' @param id A string that specifies the module's ID. The ID should be unique
#' among the app's modules.
#' @param d A reactive object containing the data to be analyzed.
#'
#' @return A Shiny module that provides a user interface and server logic for
#' a linear regression test and related plots.
#'
#' @export

linreg_module <- function(id, d) {
  moduleServer(id, function(input, output, session) {
    modns <- NS(id)
    # Create reactive objects
    ## Statistical test
    react_lm <- eventReactive(input$run_analysis,{

      lm_test <- reg_test(d(), input)
      list(lm_test$model, 
           lm_test$htmlout, 
           lm_test$report 
           )
    })
    ## Plot
    react_plotcoef <- eventReactive(input$run_analysis,{
      coef_plot <- plot_lmcoef(react_lm()[[1]])
    })
    
    react_plot <- eventReactive(input$plot_analysis,{
      lm_plots <- ggplot_lm(d(), react_lm()[[1]], input)
      lm_plots
    })
    
    # Output tables and plots
    output$text <- renderUI({
      text <- react_lm()[[3]]
      tagList(
        h2(text[1]),
        lapply(text[2:length(text)], FUN = function(x) p(x)),
      )
      
    })
    output$model <- renderUI(HTML({
      req(input$treatment != "none" & input$treatment != input$variable)

      react_lm()[[2]]
      })
      )
    output$pred_plot <- renderPlot({
      req(input$treatment != "none" & input$treatment != input$variable)
      
      react_plot()
      }#, height = 800
      )
    
    output$coefs_plot <-renderPlot(react_plotcoef()[[1]])
    output$plot_dl <- download_plot(react_plot(), "linreg", input)
    })
  
  }

#' Escapes special characters for LaTeX output
#'
#' This function takes a character string as input and escapes special characters
#' that are used in LaTeX documents such as: backslash, ampersand, percent, hash,
#' underscore, curly braces, tilde, and circumflex.
#'
#' @param s a character string to be escaped for LaTeX output
#' @return a character string with all special characters escaped for LaTeX output
#' @export
escape_for_latex <- function(s) {
  if (!is.character(s)) s_out <- as.character(s)
  else s_out <- s
  s_out <- gsub("\\", "\\textbackslash", s_out, fixed = TRUE)
  s_out <- gsub("&", "\\&", s_out, fixed = TRUE)
  s_out <- gsub("%", "\\%", s_out, fixed = TRUE)
  s_out <- gsub("#", "\\#", s_out, fixed = TRUE)
  s_out <- gsub("_", "\\_", s_out, fixed = TRUE)
  s_out <- gsub("{", "\\{", s_out, fixed = TRUE)
  s_out <- gsub("}", "\\}", s_out, fixed = TRUE)
  s_out <- gsub("~", "\\textasciitilde ", s_out, fixed = TRUE)
  s_out <- gsub("^", "\\textasciicircum ", s_out, fixed = TRUE)
  return(s_out)
}

#' reg_test
#'
#' A function to perform a regression analysis on a given dataframe, based on user-specified input parameters.
#'
#' @param d A dataframe containing the data to be analyzed.
#' @param input A list containing the following user-specified input parameters:
#' \describe{
#' \item{variable}{The dependent variable for the regression analysis.}
#' \item{treatment}{The independent variable(s) for the regression analysis. Note that this can be more than one.}
#' \item{cilevel}{The level of confidence for the confidence interval calculation.}
#' \item{allow_interaction}{A boolean value indicating whether to include interaction terms in the model.}
#' }
#'
#' @return A list containing the following elements:
#' \describe{
#' \item{model}{The regression model object.}
#' \item{htmlout}{The regression summary in HTML format, as produced by the stargazer package.}
#' \item{report}{The regression summary in text format, as produced by the reportr package.}
#' }
#' @export
reg_test <- function(d, input) {
  
  if(!is.data.frame(d)) stop("df needs to be a dataframe")
  
  # Load vars
  local_d <- copy(d)
  variable_y <- input$variable
  variable_x <- input$treatment # Note, that here, this can be more than one
  ci_level <- input$cilevel
  allow_interaction <- input$allow_interaction
  local_d <- data.table::copy(d)
  local_d[, id := 1:.N]
  
  formula_obj <- str_to_formula(variable_y, variable_x, allow_interaction)
  model <- lm(formula_obj, data = local_d)
  
  z <- summary(model)
  t_stats <- z$coefficients[, 3]
  p_vals <- z$coefficients[, 4]
  htmlout <- utils::capture.output(
    stargazer::stargazer(model, 
                         type = 'html',
                         summary = T, 
                         ci = TRUE,
                         ci.level = ci_level,
                         model.names = FALSE, 
                         single.row=TRUE
    ))
  out_report <- reportr(model)
  out <- list(model = model,
              htmlout = htmlout,
              report = out_report)
  return(out)
}