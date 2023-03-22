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
