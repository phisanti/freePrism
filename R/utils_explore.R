#' explore 
#'
#' @description A utils function
#'
#' @return The return value, if any, from executing the utility.
#'
#' @noRd

equal_sample <- function(x) {
  samples_vars <- table(x) %>% 
    unique(.) %>% 
    length(.)
  
  return(samples_vars == 1)
}

#' explore 
#'
#' @description A utils function
#'
#' @return The return value, if any, from executing the utility.
#'
#' @noRd

summary_table <- function(d, treatment){
  
  # Copy local data
  local_d <- copy(d)
  
  # Summarise data
  summ_d <- 
    local_d[, .(
      variable = colnames(local_d[, .SD, .SDcols = is.numeric]),
      N = lapply(.SD, length),
      Mean = lapply(.SD, mean, na.rm=TRUE),
      Median = lapply(.SD, median, na.rm=TRUE),
      Var = lapply(.SD, var, na.rm=TRUE),
      IQR = lapply(.SD, IQR, na.rm=TRUE)
      
    ), 
    by = treatment, 
    .SDcols = is.numeric]
  
  return(summ_d)
}

#' explore 
#'
#' @description A utils function
#'
#' @return The return value, if any, from executing the utility.
#'
#' @noRd
dist_detect <- function(d, cutoff = .05){
  
  # Copy local data
  local_d <- copy(d)
  
  dcheck <- local_d[, lapply(.SD, FUN = shapiro.test), 
                    .SDcols = is.numeric][1:2,]
  dcheck <- t(dcheck)
  dcheck <- data.table(dcheck, keep.rownames = T)
  setnames(dcheck, c('rn', 'V1', 'V2'), c('Variable', 'W-stat', 'p_val'))
  dcheck[, data_dist := fifelse(p_val > cutoff, 'norm', 'non-norm')]
  return(dcheck)
}

#' explore 
#'
#' @description A utils function
#'
#' @return The return value, if any, from executing the utility.
#'
#' @noRd
condition_ls <- function(d, treatment) {
  z <- d[, .(n_treat =lapply(.SD, FUN = function(x) length(unique(x))),
             eq_sampl = lapply(.SD, FUN = function(x) equal_sample(x)),
             n_groups = lapply(.SD, FUN = function(x) length(x) == 2)
  ), 
  .SDcols = treatment]
  
  return(z)
}

#' Stat test recommendation 
#'
#' @description A utils function
#'
#' @return The return value, if any, from executing the utility.
#'
#' @noRd
analysis_method <- function(dist_tbl, input) {
  
  # Load vars
  locald_d <- data.table::copy(dist_tbl)
  treatment_col <- input$treatment
  is_perm <- input$is_perm
  paired <- TRUE
  cond_ls <- condition_ls(d, treatment_col)
  n_treat <- unlist(cond_ls[1, "n_treat"])
  eq_sampl  <- unlist(cond_ls[1, "eq_sampl"])
  n_groups <- unlist(cond_ls[1, "n_groups"])
  
  if (is_perm == "perm") {
    out <- "Permutation"
  } else {
    locald_d[, Method := fcase(
      
      data_dist == "norm" & eq_sampl & n_treat == 2 & paired, 
      "Paired T-test",
      
      data_dist == "norm" & n_treat == 2 & !paired, 
      "T-test",
      
      data_dist == "norm" & n_treat > 2, 
      "one-way ANOVA",
      
      data_dist == "non-norm" &  n_treat == 2 & paired, 
      "Wilcoxon Signed rank test",
      
      data_dist == "non-norm" & n_treat == 2,  
      "Wilcoxon Rank Sum test", 
      default = "Kruskal-Wallis"
    )]
    
  }
  
  return(locald_d)
}
