#' linear-reg 
#'
#' @description A utils function
#'
#' @return The return value, if any, from executing the utility.
#'
#' @noRd

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

#' linear-reg 
#'
#' @description A utils function
#'
#' @return The return value, if any, from executing the utility.
#'
#' @noRd
reg_test <- function(d, input) {
  
  if(!is.data.frame(d)) stop("df needs to be a dataframe")
  
  # Load vars
  local_d <- copy(d)
  variable_y <- input$variable
  variable_x <- input$treatment # Note, that here, this can be more than one
  ci_level <- input$cilevel
  
  local_d <- data.table::copy(d)
  local_d[, id := 1:.N]
  if (length(variable_x)) {
    regressors <- paste(variable_x, collapse = " + ") 
  }
  formula_obj <- paste(variable_y, "~", regressors) %>%
    formula
  
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
                         #p = p_vals, 
                         #t = t_stats,
                         single.row=TRUE
    ))
  
  out <- list(model = model,
              htmlout = htmlout)
  return(out)
}  