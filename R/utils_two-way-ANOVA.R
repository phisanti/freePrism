#' Two-way ANOVA Shiny Module
#'
#' This module performs a two-way ANOVA and post-hoc analysis based on user-defined inputs, and
#' creates a plot to visualize the results.
#'
#' @param id The module ID.
#' @param d The data to use in the analysis.
#'
#' @export
twoway_module <- function(id, d) {
  moduleServer(id, function(input, output, session) {
    modns <- NS(id)
    
    # Create reactive objects
    ## Statistical test
    react_two_way <- eventReactive(input$run_analysis, {

      two_ANOVA <- two_ANOVA(d(), input)
      posthoc <- post_hoc_two_way(d(),
                                  input$treatment,
                                  input$variable,
                                  input$allow_interaction,
                                  input$is_normal,
                                  input$p_adjust_method)

      list(two_ANOVA[[1]], posthoc[[1]], 
           two_ANOVA[[2]], posthoc[[2]])
    })
    
    ## Plot
    reac_twowayplot <- eventReactive(input$plot_analysis, {

      twowayplot <- plot_anova(d(), input = input, 
                              #post_hoc = react_two_way()[[2]],
                           ref.group = NULL,
                           )
      list(twowayplot)
    })
    
    # Output tables and plots
    output$test_text <- renderUI({
      text <- react_two_way()[[3]]
      tagList(
        h2(text[1]),
        lapply(text[2:length(text)], FUN = function(x) p(x)),
        h3("Summary statistic table")
      )
    })
    output$test <- renderDT({
      req(input$treatment != "" & input$treatment != input$variable)
      react_two_way()[[1]] %>%
        round_siginf_table
    })
    output$posthoc_text <- renderUI({
      text <- react_two_way()[[4]]
      tagList(
        h2(text[1]),
        lapply(text[2:length(text)], FUN = function(x) p(x)),
        h3("Summary statistic table")
      )
    })
    output$posthoc <- renderDT({
      req(input$treatment != "" & input$treatment != input$variable)
      #req()
      react_two_way()[[2]] %>%
        round_siginf_table
    })
    output$plot <- renderPlot({
      req(input$xvar != "" & input$yvar != "")

      reac_twowayplot()[[1]]
    })
    output$plot_dl <- download_plot(reac_twowayplot()[[1]], id)
    
    })
  }

#' two-way-ANOVA 
#'
#' @description A utils function
#'
#' @return The return value, if any, from executing the utility.
#'
#' @noRd
two_ANOVA <- function(d, input) {
  
  # Load vars
  . <- NULL
  treatment <- input$treatment 
  variable  <- input$variable
  allow_interaction <- input$allow_interaction
  is_normal <- input$is_normal
  ANOVA_type <- input$anova_type
  
  local_d <- data.table::copy(d)
  local_d[, id := 1:.N]
  local_d[, (treatment) := lapply(.SD, as.factor), 
          .SDcols = (treatment)]
  local_d[, paste0("ranked_", variable) := lapply(.SD, rank), 
          .SDcols = variable]
  types_anova <- c("I" = 1, "II" = 2,"III"= 3)
  seleted_anova <- types_anova[ANOVA_type]
  
  if (is_normal) {
    stat_method <- "two-way-ANOVA"
  } else {
    stat_method <- "ranked-two-way-ANOVA"
    }
    
  # Create formula object
  formula_obj <-  str_to_formula(variable, treatment, 
                                 allow_interaction, is_normal)
  # Run ANOVA

  out <- local_d %>%
    anova_test(formula_obj, data = ., wid ="id",  detailed = T, type = seleted_anova)
  out$method <- stat_method
  
  # Generate Report
  report_out <- reportr(out)

  return(list(out, report_out))
}

#' two-way-ANOVA 
#'
#' @description A utils function
#'
#' @return The return value, if any, from executing the utility.
#'
#' @noRd
post_hoc_two_way <- function(d, 
                             treatment, 
                             variable, 
                             allow_interaction = FALSE, 
                             is_normal = FALSE, 
                             p.adjust.method = "bonferroni") {
  
  # Load vars
  . <- NULL
  local_d <- data.table::copy(d)
  local_d[, id := 1:.N]
  local_d[, (treatment) := lapply(.SD, as.factor), 
          .SDcols = treatment]
  local_d[, paste0("ranked_", variable) := lapply(.SD, rank), 
          .SDcols = variable]
  
  if (allow_interaction) {
    collapse_char <- "*"
  } else {
    collapse_char <- "+"
  }
  
  treatment_str <- paste(treatment, collapse = collapse_char)
  
  if (is_normal) {
    target_variable <- variable
    stat_method <- "two-way-ANOVA"
  } else {
    target_variable <- paste0("ranked_", variable)
    stat_method <- "ranked-two-way-ANOVA"
    
  }
  formula_obj <- paste(target_variable, "~", treatment_str) %>%
    formula  
  # Run test
  
  out <- local_d %>% 
    tukey_hsd(x = ., formula = formula_obj, 
              p.adjust.method = p.adjust.method)
  
  report_out <- reportr(out)
  
  return(list(out, report_out))
}
