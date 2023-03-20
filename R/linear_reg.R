# escape_for_latex <- function(s) {
#   if (!is.character(s)) s_out <- as.character(s)
#   else s_out <- s
#   s_out <- gsub("\\", "\\textbackslash", s_out, fixed = TRUE)
#   s_out <- gsub("&", "\\&", s_out, fixed = TRUE)
#   s_out <- gsub("%", "\\%", s_out, fixed = TRUE)
#   s_out <- gsub("#", "\\#", s_out, fixed = TRUE)
#   s_out <- gsub("_", "\\_", s_out, fixed = TRUE)
#   s_out <- gsub("{", "\\{", s_out, fixed = TRUE)
#   s_out <- gsub("}", "\\}", s_out, fixed = TRUE)
#   s_out <- gsub("~", "\\textasciitilde ", s_out, fixed = TRUE)
#   s_out <- gsub("^", "\\textasciicircum ", s_out, fixed = TRUE)
#   return(s_out)
# }
# 
# 
# reg_test <- function(d, input) {
#   
#   if(!is.data.frame(d)) stop("df needs to be a dataframe")
#   
#   # Load vars
#   local_d <- copy(d)
#   variable_y <- input$variable
#   variable_x <- input$treatment # Note, that here, this can be more than one
#   ci_level <- input$cilevel
#   
#   local_d <- data.table::copy(d)
#   local_d[, id := 1:.N]
#   if (length(variable_x)) {
#     regressors <- paste(variable_x, collapse = " + ") 
#   }
#   formula_obj <- paste(variable_y, "~", regressors) %>%
#     formula
#   
#   model <- lm(formula_obj, data = local_d)
#   
#   z <- summary(model)
#   t_stats <- z$coefficients[, 3]
#   p_vals <- z$coefficients[, 4]
#   htmlout <- utils::capture.output(
#     stargazer::stargazer(model, 
#                          type = 'html',
#                          summary = T, 
#                          ci = TRUE,
#                          ci.level = ci_level,
#                          model.names = FALSE, 
#                          #p = p_vals, 
#                          #t = t_stats,
#                          single.row=TRUE
#                          ))
#   
#   out <- list(model = model,
#               htmlout = htmlout)
#    return(out)
# }  
# 
# ggplot_lm <- function(d, model, input) {
#   
#   # Shape data
#   local_d <- copy(d)
#   xvar <- input$xvar
#   yvar <- input$yvar
#   
#   if ("colvar" %in% names(input) && input$colvar != "") {
#     
#     colvar <- input$colvar
#     local_d[, c("xvar", "yvar", "colvar") := .SD, 
#             .SDcols = c(xvar, yvar, colvar)]
#     col_var_present <- T
#   } else  {
#     
#     local_d[, c("xvar", "yvar") := .SD, 
#             .SDcols = c(xvar, yvar)]
#     local_d[, colvar := as.factor(0)]
#     col_var_present <- F
#   }
#   
#   coef_table <- summary(model) %>%
#     coefficients %>%
#     data.table(., keep.rownames = T)
#   setnames(coef_table, c("variable", "estimate", "error", "t_val", "p_val"))
#   coef_table[, p_val_label := signif(p_val, 3)]
#   
#   # make plot effects
#   ggp1 <- ggplot(coef_table, aes(x = estimate, y = variable)) +
#     geom_point() +
#     geom_errorbarh(aes(xmin = estimate - error, xmax = estimate + error), 
#                    height = .25) +
#     geom_text(aes(y = variable, label = p_val_label),
#               vjust = -3) +
#     geom_vline(xintercept = 0, linetype = 2, col = "firebrick") +
#     theme_pubr()
#   
#   # Plot 
#   local_d[, pred_vals := fitted(model)]
#   ggp2 <- ggplot(local_d, 
#          aes(x = xvar)) +
#     geom_point(aes(y = yvar, col = colvar), show.legend = col_var_present) +
#     geom_point(aes(y = pred_vals), col = "firebrick")
#     
# 
#   return(list(ggp1 = ggp1, ggp2 = ggp2))
# }
# #ggplot_lm(d, model, input)
