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

      two_ANOVA <- two_ANOVA(d(),
                             input$treatment,
                             input$variable,
                             input$allow_interaction,
                             input$is_normal,
                             ANOVA_type = input$anova_type)
      posthoc <- post_hoc_two_way(d(),
                                  input$treatment,
                                  input$variable,
                                  input$allow_interaction,
                                  input$is_normal,
                                  input$p_adjust_method)

      list(two_ANOVA, posthoc)
    })
    
    ## Plot
    reac_twowayplot <- eventReactive(input$plot_analysis, {

      twaplot <- plot_two_way(d(),
                           input$xvar,
                           input$yvar,
                           input$colvar,
                           post_hoc = react_two_way()[[2]],
                           ref.group = NULL,
                           plot_type = input$plot_type,
                           col_palette = input$colpal)
      list(twaplot)
    })
    
    # Output tables and plots
    output$test <- renderDT({
      req(input$treatment != "" & input$treatment != input$variable)
      react_two_way()[[1]]
    })
    output$posthoc <- renderDT({
      req(input$treatment != "" & input$treatment != input$variable)
      react_two_way()[[2]]
    })
    output$plot <- renderPlot({
      req(input$xvar != "" & input$yvar != "")

      reac_twowayplot()[[1]]
    })
    
    })
  }

#' two-way-ANOVA 
#'
#' @description A utils function
#'
#' @return The return value, if any, from executing the utility.
#'
#' @noRd
two_ANOVA <- function(d, 
                      treatment, 
                      variable, 
                      allow_interaction = FALSE, 
                      is_normal = FALSE, 
                      ANOVA_type = 2) {
  
  # Load vars
  . <- NULL
  local_d <- data.table::copy(d)
  local_d[, id := 1:.N]
  local_d[, (treatment) := lapply(.SD, as.factor), 
          .SDcols = (treatment)]
  local_d[, paste0("ranked_", variable) := lapply(.SD, rank), 
          .SDcols = variable]
  types_anova <- c("I" = 1, "II" = 2,"III"= 3)
  seleted_anova <- types_anova[ANOVA_type]

  # Create formula object
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
  # Run ANOVA

  out <- local_d %>%
    anova_test(formula_obj, data = ., wid ="id",  detailed = T, type = seleted_anova) %>% 
    get_anova_table(.) %>%
    data.table
  
  out[, method := stat_method]

  return(out)
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
  
  out <- aov(formula_obj, data = local_d) %>% 
    tukey_hsd(., p.adjust.method = p.adjust.method) %>% 
    data.table
  
  return(out)
}
