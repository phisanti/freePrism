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
        list(one_way_test[[1]], one_way_test[[2]])
      })
    
    react_plot <- eventReactive(input$plot_analysis, {
        test_plot <- plot_one_way(d(),
                     variable = input$variable,
                     treatment = input$treatment,
                     post_hoc = react_test()[[2]],
                     ref.group = NULL,
                     plot_type = input$plot_type,
                     col_palette = input$colpal)
      list(test_plot)
    })
    
    # Output tables and plots
    output$one_way_test <- renderDT({
      req(input$treatment != "" & input$treatment != input$variable)

      react_test()[[1]]
    })
    output$one_way_post <- renderDT({
      req(input$treatment != "" & input$treatment != input$variable)

      react_test()[[2]]
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
  formula_obj <- paste(variable, "~", treatment) %>%
    formula
  # Run ANOVA
  out <- local_d %>%
    anova_test(formula_obj, data = .,wid ="id",  detailed = T, type = ANOVA_type) %>% 
    get_anova_table(.) %>%
    data.table
  out[, method := "One-Way-ANOVA"]
  
  return(out)
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
  formula_obj <- paste(variable, "~", treatment) %>%
    formula(.)
  # Run Test
  out <- local_d %>%
    kruskal_test(formula_obj, data = .)
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
  return(out_table)
}

#' one-way-ANOVA 
#'
#' @description A utils function
#'
#' @return The return value, if any, from executing the utility.
#'
#' @noRd
post_hoc_aov <- function(d, treatment, variable, p.adjust.method = "bonferroni") {
  
  # Load vars
  local_d <- copy(d)
  formula_obj <- paste(variable, "~", treatment) %>%
    formula(.)
  
  # Run test
  out <- aov(formula_obj, data = local_d) %>% 
    tukey_hsd(., p.adjust.method = p.adjust.method) %>% 
    data.table
  
  return(out)
}

#' KS post hoc
#'
#' @description A utils function
#'
#' @return The return value, if any, from executing the utility.
#'
#' @noRd
post_hoc_kw <- function(d, treatment, variable, p.adjust.method = "bonferroni") {
  
  # Load vars
  local_d <- copy(d)
  formula_obj <- paste(variable, "~", treatment) %>%
    formula(.)
  # Run Test
  out <- local_d %>%
    dunn_test(., formula_obj, 
              p.adjust.method = p.adjust.method, 
              detailed = FALSE) %>%
    data.table
  
  return(out)
  
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
  local_d <- copy(d)
  local_d[, (treatment) := lapply(.SD, as.factor), 
          .SDcols = treatment]
  
  if (input$one_test == "One-Way-ANOVA") {
    one_test <- one_ANOVA(local_d, treatment = treatment,variable =  variable, ANOVA_type = 2)
    one_post_hoc <- post_hoc_aov(local_d, treatment = treatment,variable =  variable, p.adjust.method = p_adjust)
    
  } else {
    one_test <- K_wallis(local_d, treatment = treatment, variable = variable)
    one_post_hoc <- post_hoc_kw(local_d, treatment = treatment, variable = variable,  p.adjust.method = p_adjust)
    
  }
  
  out <- list(one_test, one_post_hoc) 
  return(out)
}
