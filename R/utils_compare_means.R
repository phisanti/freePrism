#' my_t_test 
#'
#' @description A groups of functions to perform two-sample comparisons
#'
#' @return The return value, if any, from executing the utility.
#'
#' @noRd

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
  
  test_out <- t_test(formula = formula_obj, 
                     data = local_d,
                     detailed = T,
                     ref.group = ref.group,
                     paired = paired, 
                     var.equal = var_equal,
                     alternative = alt_h
  ) %>%
    data.table
  col_names <- colnames(test_out)
  col_remove <- col_names %in% c("n1", "n2", "conf.low", "conf.high", 
                                 "alternative", "p.adj.signif")
  test_out <- test_out[, -col_names[col_remove],
                       with = FALSE]
  if ("estimate1" %in% col_names) {
    setcolorder(test_out, c(".y.", "group1","estimate1", "group2","estimate2", 
                            "estimate"))
  } else {
    setcolorder(test_out, c(".y.", "group1", "group2", 
                            "estimate"))
    
  }
  return(test_out)
}

#' my_w_test 
#'
#' @description A groups of functions to perform two-sample comparisons
#'
#' @return The return value, if any, from executing the utility.
#'
#' @noRd
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
  
  test_out <- wilcox_test(formula = formula_obj, 
                          data = local_d,
                          detailed = T,
                          ref.group = ref.group,
                          paired = paired, 
                          alternative = alt_h
  ) %>%
    data.table
  col_remove <- colnames(test_out) %in% c("n1", "n2", "conf.low", "conf.high", 
                                          "alternative", "p.adj.signif")
  test_out <- test_out[, -colnames(test_out)[col_remove], 
                       with = FALSE]
  setcolorder(test_out, c(".y.", "group1", "group2", 
                          "estimate"))
  return(test_out)
}

#' my_w_test 
#'
#' @description A groups of functions to perform two-sample comparisons
#'
#' @return The return value, if any, from executing the utility.
#'
#' @import data.table rstatix ggplot2
#' @noRd
#' @export

comp_means_test <- function(d, input){

  # Load variables
  treatment <- input$treatment
  variable <- input$variable
  paired <- input$paired
  ref.group = input$ref.group
  alt_h = input$alt_h
  
  # Perform chosen test
  if (input$mean_test == "T-test") {
    var_equal = input$var_equal == TRUE
    test_out <- my_t_test(d,
                          treatment, 
                          variable, 
                          ref.group = ref.group,
                          paired = paired, 
                          var_equal = var_equal, 
                          alt_h = alt_h)
  } else if (input$mean_test == "Wilcox-test") {
    test_out <- my_w_test(d,
                          treatment, 
                          variable, 
                          ref.group = ref.group,
                          paired = FALSE, 
                          alt_h = alt_h)
    
  } else {
    test_out <- data.table(Issue = "Selected test not found")
  }
  return(test_out)
}

