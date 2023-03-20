# Aux functions

get_aov_data <- function(aov_summary) {
  aov_data <- aov_summary[[1]]
  rel_data <- data.table(
    statistic = aov_data[1, "F value"],
    df = aov_data[1, "Df"],
    p.value = aov_data[1, "Pr(>F)"],
    method = "one-way ANOVA"
  )
  
  return(rel_data)
}



########################################
# 1. compare data across treatments,
#    the comparison method was
#    determined by rct_analysis_method.
#    This reactive function is called
#    rct_compare_ls
#    return: tibble
#    render: DT - with Buttons
########################################

compare_ls <- function(d, 
                       treatment, 
                       dist_table, 
                       man_var_method = NULL) {
  
  # Load vars
  local_dist <- copy(dist_table)
  local_d <- copy(d)
  
  # Set options for statistical tests
  t_test_fam <- list(variants = c("T-test (unequal variance)",
                              "T-test (equal variance)",
                              "Paired T-test"),
                     "T-test (unequal variance)" = list(
                      paired_arg = FALSE,
                      var_equal_arg = FALSE),
                     "T-test (equal variance)" = list(
                       paired_arg = FALSE,
                       var_equal_arg = TRUE),
                     "Paired t-test" = list(
                       paired_arg = TRUE,
                       var_equal_arg = FALSE
                     )
                     )
  wilcox_fam <- list(variants = c("Wilcoxon Signed rank test",
                              "Wilcoxon Rank Sum test"), 
                    "Wilcoxon Signed rank test" = list(
                      paired_arg = TRUE
                    ),
                    "Wilcoxon Rank Sum test" = list(
                      paired_arg = FALSE
                    )
                     )
  n <- nrow(local_dist)
  all_test <- vector("list", n)
  # Iterate over vars
  for (i in seq_len(n)) {
    
    # Get var and method
    var_name <- local_dist[i, Variable]
    if (is.null(man_var_method)) {
      var_method <- local_dist[i, Method]
    } else {
      var_method <- man_var_method
    }
    
    # Get data
    var_data <- local_d[, var_name, with = FALSE] %>% 
      unlist(., use.names = FALSE)
    treat_data <- local_d[, treatment, with = FALSE] %>% 
      unlist(., use.names = FALSE)
    
    # Perform selected test and get p-vals
    if (var_method == "Permutation test") {
      
    } else if (var_method %in% t_test_fam$variants) {
      
      test_args = t_test_fam[[var_method]]
      out <- t.test(var_data ~ treat_data, 
             na.action = na.omit, 
             paired = test_args$paired_arg, 
             var.equal = test_args$var_equal_arg
             ) %>%
        .[c("statistic",
            "p.value",
            "parameter",
            "method")] %>%
        as.data.table()
      out[, variable := var_name]
      
    } else if (var_method %in%  wilcox_fam$variants) {
      
      test_args = wilcox_fam[[var_method]]
      out <- wilcox.test(var_data ~ treat_data, 
                  na.action = na.omit, 
                  paired = test_args$paired_arg) %>%
        .[c("statistic",
            "p.value",
            "parameter",
            "method")] %>%
        as.data.table()
      out[, variable := var_name]
      
    } else if (var_method == "Kruskal-Wallis") {
      
      out <- kruskal.test(var_data ~ treat_data, 
                   na.action = na.omit) %$%
        .[c("statistic",
               "p.value",
               "parameter",
               "method")] %>% 
        as.data.table(.)
        setnames(out, "parameter", "df")
        out[, variable := var_name]
        
    } else if (var_method == "one-way ANOVA") {
      out <- aov(var_data ~ treat_data, 
          na.action = na.omit) %>%
        summary(.) %>%
        get_aov_data
      out[, variable := var_name]
      
    }
    
    # Collect test in list
    all_test[[i]] <- out
    
  }
  # Join outputs and return
  joint_table_tests <- rbindlist(all_test, use.names= T)
  setcolorder(joint_table_tests, "variable")
  return(joint_table_tests)

}

########################################
# 1. compare data across treatments,
#    the comparison method was
#    determined by rct_analysis_method.
#    This reactive function is called
#    rct_compare_ls
#    return: tibble
#    render: DT - with Buttons
########################################

compare_mult_gr <- function(d, dist_table, treatment, var_method) {
  
  # Load vars
  local_dist <- copy(dist_table)
  local_d <- copy(d)
  
  # Set options for statistical tests
  
  n <- nrow(local_dist)
  all_test <- vector("list", n)
  # Iterate over vars
  for (i in seq_len(n)) {
    
    # Get var and method
    var_name <- local_dist[i, Variable]
    var_method <- local_dist[i, Method]
    
    # Get data
    var_data <- local_d[, var_name, with = FALSE] %>% 
      unlist(., use.names = FALSE)
    treat_data <- local_d[, treatment, with = FALSE] %>% 
      unlist(., use.names = FALSE)
    
    # Perform selected test and get p-vals
    if (var_method == "Permutation test") {
      

    } else if (var_method == "Kruskal-Wallis") {
      
      out <- kruskal.test(var_data ~ treat_data, 
                          na.action = na.omit) %$%
        .[c("statistic",
            "p.value",
            "parameter",
            "method")] %>% 
        as.data.table(.)
      setnames(out, "parameter", "df")
      out[, variable := var_name]
      
    } else if (var_method == "one-way ANOVA") {
      out <- aov(var_data ~ treat_data, 
                 na.action = na.omit) %>%
        summary(.) %>%
        get_aov_data
      out[, variable := var_name]
      
    }
    
    # Collect test in list
    all_test[[i]] <- out
    
  }
  # Join outputs and return
  joint_table_tests <- rbindlist(all_test, use.names= T)
  setcolorder(joint_table_tests, "variable")
  return(joint_table_tests)
  
}


########################################
# 2. compare data across treatments,
#    the comparison method was
#    determined by rct_analysis_method.
#    This reactive function is called
#    rct_compare_ls
#    return: tibble
#    render: DT - with Buttons
########################################


########################################
# 2. Post-Hoc Tests
#    the comparison method was
#    determined by rct_analysis_method.
#    This reactive function is called
#    rct_gg_post_hoc
#    return: ggobject
#    render: plot
#
#   Customized parameter
#
#       xlab = input$plot_x_lab,
#       ylab = input$plot_y_lab,
#       pairwise.display = input$pairwise_display,
#       pairwise.annotation = input$pairwise_annotation,
#       title.prefix = input$title_prefix,
########################################

post_hoc <- function(d, treatment, post_hoc_method) {
  
  local_d <- copy(d)
  
  num_cols <- d[, .SD, .SDcols = is.numeric] %>% 
    colnames()
  melt_d <- local_d[, c(treatment, num_cols), with = F] %>%
    melt.data.table(., id.vars = treatment)
  
  
  return(TRUE)
}
