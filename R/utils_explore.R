
explore_module <- function(id, d) {
  moduleServer(id, function(input, output, session) {
    modns <- NS(id)
    
    react_explore <- eventReactive(input$run_analysis,{

      react_dist_tbl <- dist_detect(d(), 0.05)
      react_summ <- summary_table(d(), input$treatment)

      list(react_summ, react_dist_tbl)
      
    })
    react_eplot <- eventReactive(input$plot_analysis,{
      gplot <- ggplot_explore(d(), input)
      list(gplot)
    })
    
    output$summ <- renderDT({
      react_explore()[[1]] %>%
        round_siginf_table})
    output$dist_tbl <- renderDT({
      react_explore()[[2]] %>%
        round_siginf_table
      })
    
    output$plot <- renderPlot({
      req(input$treatment %in% colnames(d())| input$treatment == "none")
      
      react_eplot()
    })
  })
}

#' Check if all elements in a vector have equal frequency
#'
#' @param x A vector of values
#'
#' @return A logical value indicating whether all elements in the input vector have equal frequency
#'
#' @export
#' @noRd
equal_sample <- function(x) {
  samples_vars <- table(x) %>% 
    unique(.) %>% 
    length(.)
  
  return(samples_vars == 1)
}

#' Summarize data by treatment group
#'
#' This function takes a data.table and a treatment variable and returns a summary table of the numeric variables in the data.table, grouped by the treatment variable. The summary statistics calculated are: sample size (N), mean, median, variance (Var), and interquartile range (IQR).
#'
#' @param d A data table
#' @param treatment A character string specifying the name of the treatment variable in the data table
#'
#' @return A summary table of the numeric variables in the input data table, grouped by the treatment variable
#' @export
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

#' Detect the distribution of numeric variables in a data.table
#'
#' This function takes a data.table and an optional cutoff value as input and returns a data.table with the results of the Shapiro-Wilk test for normality for each numeric variable in the input data.table. The output data.table contains the variable name, W-statistic, p-value, and a column indicating whether the variable is normally distributed or not based on the specified cutoff value.
#'
#' @param d A data table
#' @param cutoff An optional numeric value specifying the p-value cutoff for determining normality (default is 0.05)
#'
#' @return A data table with the results of the Shapiro-Wilk test for normality for each numeric variable in the input data table
#' @export
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
