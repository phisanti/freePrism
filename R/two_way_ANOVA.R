# The following scripts contains all the functions required to run one-way ANOVA
# and it's non-parametric equivalent.

# Test functions
## One-way anova test function
two_ANOVA <- function(d, treatment, variable, is_nonparametric = FALSE, ANOVA_type = 2) {
  
  # Load vars
  . <- NULL
  local_d <- data.table::copy(d)
  local_d[, id := 1:.N]
  local_d[, (treatment) := lapply(.SD, as.factor), 
          .SDcols = treatment]
  local_d[, paste0("ranked_", variable) := lapply(.SD, rank), 
          .SDcols = variable]
  treatment_str <- paste(treatment, collapse = "+")
  if (is_nonparametric) {
    target_variable <- paste0("ranked_", variable)
    stat_method <- "ranked-two-way-ANOVA"
  } else {
    target_variable <- variable
    stat_method <- "two-way-ANOVA"
      }
  formula_obj <- paste(target_variable, "~", treatment_str) %>%
    formula
  # Run ANOVA
  out <- local_d %>%
    anova_test(formula_obj, data = ., wid ="id",  detailed = T, type = ANOVA_type) %>% 
    get_anova_table(.) %>%
    data.table
  
  out[, method := stat_method]
  print(out)
  
  return(out)
}

# Post hocs
## ANOVA post hoc
post_hoc_two_way <- function(d, 
                         treatment, 
                         variable, 
                         is_nonparametric, 
                         p.adjust.method = "bonferroni") {
  
  # Load vars
  local_d <- copy(d)  
  treatment_str <- paste(treatment, collapse = "+")
  formula_obj <- paste(variable, "~", treatment_str) %>%
    formula(.)
  
  # Run test

  out <- tukey_hsd(data = local_d, formula = formula_obj, p.adjust.method = p.adjust.method) %>% 
      data.table

  return(out)
}
