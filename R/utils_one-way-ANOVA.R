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
