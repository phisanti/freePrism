
########################################
# 0. Aux functions
#    return p value
#    return: data.table
#    render: DT
########################################

equal_sample <- function(x) {
  samples_vars <- table(x) %>% 
    unique(.) %>% 
    length(.)
  
  return(samples_vars == 1)
}

########################################
# 0. Normality check via Shapiro-Wilk Normality Test
#    return p value
#    return: data.table
#    render: DT
########################################

df_sw_test <- shiny::reactive({
  d <- data.table(iris)
  sw <- d[, lapply(.SD, FUN = shapiro.test), .SDcols = is.numeric][1:2,]
  
  return(sw)
})

df_sw_test <- function(d = NULL){
  if (is.null(d)) {d <- data.table(iris)}
  sw <- d[, lapply(.SD, FUN = shapiro.test), .SDcols = is.numeric][1:2,]
  
  return(sw)
}


########################################
# 1. detect the distribution by sw_pv
#    This reactive function is called
#    df_data
#    return: data.table
#    render: DT
########################################

# Original version
dist_detect <- reactive({
  
  cutoff <- input$x
  dcheck <- df_sw_test()
  
  dcheck <- t(dcheck)
  dcheck <- data.table(dcheck, keep.rownames = T)
  setnames(dcheck, c('rn', 'V1', 'V2'), c('Variable', 'W-stat', 'p_val'))
  dcheck[, data_dist := fifelse(p_val > cutoff, 'norm', 'non-norm')]
  
  return(dcheck)
})

# Alternative writing of the same function
dist_detect <- function(d, cutoff = .05){
  
  dcheck <- df_sw_test(d)
  dcheck <- t(dcheck)
  dcheck <- data.table(dcheck, keep.rownames = T)
  setnames(dcheck, c('rn', 'V1', 'V2'), c('Variable', 'W-stat', 'p_val'))
  dcheck[, data_dist := fifelse(p_val > cutoff, 'norm', 'non-norm')]
  return(dcheck)
}

########################################
# 2. To determine wheter to use
#    parametric test or nonparametric
#    test.
#
#    Once a skewed distribution is
#    found in each
#    variable, the parametric test is
#    not suitable any more.
#    This section is seperated into
#    two reactive functions.
#    condition prepare conditions
#    analysis_method generate tibble
#    return: tibble
#    render: DT
########################################

condition_ls <- function(d, treatment) {
  z <- d[, .(n_treat =lapply(.SD, FUN = function(x) length(unique(x))),
        eq_sampl = lapply(.SD, FUN = function(x) equal_sample(x)),
        n_groups = lapply(.SD, FUN = function(x) length(x) == 2)
  ), 
  .SDcols = treatment]

  return(z)
}
  
# Stat test recommendation

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

########################################
# 2.5. apply Levene’s test to check
#    variance homogeneity
#    This reactive function is called
#    levene_p
#    return: tibble
#    render: DT
#    colnames: Levene's Test (p.value)
########################################

levene_p <- function(d, treatment){
  
  local_d <- data.table::copy(d)
  btest_res <- local_d[, lapply(.SD, FUN = function(x, treatment) {
    bartlett.test(x, treatment)
  }, treatment = get(treatment)
  ), .SDcols = is.numeric][1:3,]
  btest_res <- t(btest_res) %>%
    data.table() %>%
    setNames(., c("statistic", "parameter", "p_val"))
  
  return(btest_res)

}

########################################
# 4. we plot the histgram of each
#    variable.
#    This reactive function is called
#    ggplot_hist
#    return: ggobject
#    render: plot
#    structure:
#       wrap: name (Variable)
#       axes: both x and y are free
########################################

ggplot_hist <- function(d, treatment, group = NULL) {
  
  # Load vars
  local_d <- copy(d)
  num_cols <- d[, .SD, .SDcols = is.numeric] %>% 
    colnames()
  group_is_correct <- group %in% colnames(local_d)
  # Reshape data
  melt_d <- local_d[, c(treatment, num_cols), with = F] %>%
    melt.data.table(., id.vars = treatment)
  
  # Plot data
  if (is.null(group)) {
    ggplot(melt_d, aes(x = value)) +
      geom_histogram(fill = "cornflowerblue") +
      facet_wrap(variable ~ ., scales = "free") +
      theme_classic()
  } else if (group_is_correct) {
    ggplot(melt_d, aes(x = value)) +
      geom_histogram(aes(fill = get(group))) +
      facet_wrap(variable, scales = "free") +
      theme_classic()
    
  } else {
    
  }
}

########################################
# 6. we plot the qqplot of each
#    variable.
#    This reactive function is called
#    rct_ggplot_qq
#    return: ggobject
#    render: plot
#    structure:
#       wrap: name (Variable)
#       axes: both x and y are free
########################################

ggplot_qq <- function(d, treatment) {
  
  # Load vars
  local_d <- copy(d)
  num_cols <- d[, .SD, .SDcols = is.numeric] %>% 
    colnames()
  # Reshape data
  melt_d <- local_d[, c(treatment, num_cols), with = F] %>%
    melt.data.table(., id.vars = treatment)
  
  # Plot
  ggplot(melt_d, aes(sample = value)) +
    stat_qq() +
    stat_qq_line() +
    facet_wrap(variable ~ .,
               scales = "free"
    ) +
    theme_classic()
}

########################################
# 7. Provide options to determine
#    what kind of statics
#    would you like manually
#    This reactive function is called
#    rct_var_select_method_determine
#    return: uiOutput
########################################

# This function is still not completed, the original allows to iterate
# over the variables and methods
select_manual_method <- function(variable, manual_method) {
  
  # Load vars
  stat_methods <- list(
    two_groups = c(
    "Wilcoxon Rank Sum test",
    "t-test (unequal variance)",
    "t-test (equal variance)"
  ),
  more_than_two = c(
    "Kruskal Wallis H-test",
    "one-way ANOVA"
  ),
  paired = c(
    "Wilcoxon Signed rank test",
             "Paired t-test"
    ),
  perm = "Permutation test"
  )
  
  # Get selection
  if (manual_method %in% stat_methods$two_groups) {
    selected_method <- stat_methods$two_groups
  } else if (manual_method %in% stat_methods$more_than_two) {
    selected_method <- stat_methods$more_than_two
    
  } else if (manual_method %in% stat_methods$paired) {
    selected_method <- stat_methods$paired
  } else {
    selected_method <- stat_methods$perm
  }
  
  # Show data
  column(
    width = 4,
    selectInput(
      inputId = paste0("var_", variable),
      label = paste("Method for", variable),
      choices = selected_method,
      selected = manual_method
    )
  )
  
}
