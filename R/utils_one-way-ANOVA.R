#' One-Way ANOVA Module
#'
#' @description This module performs a one-way analysis of variance (ANOVA) on the given data
#' and generates a plot of the results.
#'
#' @param id A character string that specifies the namespace for the module.
#' @param d A reactive expression that returns the data to be used in the analysis.
#'
#' @return A module server function that can be called within a Shiny application.
#'
#' @export
oneway_module <- function(id, d) {
  moduleServer(id, function(input, output, session) {
    
    # Load vars
    modns <- NS(id)
    
    # Create reactive objects
    react_test <- eventReactive(input$run_analysis,{
      
        one_way_test <- one_way_test(d(), input)
        list(one_way_test[[1]], one_way_test[[2]],
             one_way_test[[3]], one_way_test[[4]])
      })
    
    react_plot <- eventReactive(input$plot_analysis, {
      onewayplot <- plot_anova(d(), input = input, 
                               ref.group = NULL,
      )
      list(onewayplot)
    })
    
    # Output tables and plots
    output$test_text <- renderUI({
      text <- react_test()[[3]]
      tagList(
        h2(text[1]),
        p(text[2]),
        p(text[3]),
        h3("Summary statistic table")
      )
    })
    
    output$one_way_test <- renderDT({
      req(input$treatment != "" & input$treatment != input$variable)

      react_test()[[1]] %>%
        round_siginf_table
    })
    
    output$posthoc_text <- renderUI({
      text <- react_test()[[4]]
      tagList(
        h2(text[1]),
        lapply(text[2:length(text)], FUN = function(x) p(x)),
        h3("Summary statistic table")
      )
    })
    
    output$one_way_post <- renderDT({
      req(input$treatment != "" & input$treatment != input$variable)

      react_test()[[2]] %>%
        round_siginf_table
    })
    output$one_way_plot <- renderPlot({
      req(input$treatment != "" & input$treatment != input$variable)
      react_plot()[[1]]
    })
    
    })
  }

#' one-way-ANOVA 
#'
#' @description A utils function
#'
#' @return The return value, if any, from executing the utility.
#'
#' @noRd
one_ANOVA <- function(d, treatment, variable, ANOVA_type = 2) {
  
  # Load vars
  . <- NULL
  local_d <- data.table::copy(d)
  local_d[, id := 1:.N]
  formula_obj <- str_to_formula(variable, treatment)
  
  # Run ANOVA
  out <- local_d %>%
    anova_test(formula_obj, data = .,wid ="id",  detailed = T, type = ANOVA_type)
  
  out_report <- reportr(out)
  attr_out <- attr(out, "args")
  out <-  data.table(out)
  setattr(out, "args", attr_out) 
  out[, method := "One-Way-ANOVA"]
  
  return(list(test = out, report = out_report))
}

#' one-way-ANOVA 
#'
#' @description A utils function
#'
#' @return The return value, if any, from executing the utility.
#'
#' @noRd
K_wallis <- function(d, treatment, variable) {
  
  # Load vars
  local_d <- copy(d)
  local_d[, id := 1:.N]
  formula_obj <- str_to_formula(variable, treatment)
  
  # Run Test
  out <- local_d %>%
    kruskal_test(data = ., formula = formula_obj)
  
  out_report <- reportr(out)
  e_s <- kruskal_effsize(formula_obj, data = local_d)
  out_table <- data.table(Variable = variable,
                          treatment = treatment,
                          n = out$n,
                          statistic = out$statistic,
                          df = out$df,
                          p = out$p,
                          stat_method = out$method,
                          effect_size = e_s$effsize,
                          eff_method = e_s$method,
                          method = "Kruskall-Wallis")
  return(list(test = out, report = out_report))
}

#' One-way Test
#'
#' @description A utils function to combine the parametric and non-parametric
#' versions of the One-way ANOVA
#'
#' @return The return value, if any, from executing the utility.
#'
#' @noRd
one_way_test <- function(d, input) {
  
  # Load variables
  treatment <- input$treatment
  variable <- input$variable
  p_adjust <- input$p_adjust_method
  posthoc <- input$posthoc
  anova_type <- input$anova_type
  local_d <- copy(d)
  local_d[, (treatment) := lapply(.SD, as.factor), 
          .SDcols = treatment]
  
  # Convert ANOVA type to numeric
  anova_type <- c("I" = 1, "II" = 2, "III" = 3)[anova_type]
  
  # Select test
  if (input$test == "One-Way-ANOVA") {
    
    one_test <- one_ANOVA(local_d, treatment = treatment,variable =  variable,
                          ANOVA_type = anova_type)
    one_post_hoc <- posthoc_test(local_d, treatment = treatment,variable =  variable,
                                 test = posthoc, p.adjust.method = p_adjust)
  } else {
    one_test <- K_wallis(local_d, treatment = treatment, variable = variable)
    one_post_hoc <- posthoc_test(local_d, treatment = treatment,variable =  variable,
                                 test = posthoc, p.adjust.method = p_adjust)
  }
  
  out <- list(test = one_test$test, 
              posthoc = one_post_hoc$posthoc, 
              test_report = one_test$report, 
              posthoc_report = one_post_hoc$posthoc_report) 
  return(out)
}
