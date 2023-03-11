
########################################
#   One-Way test
#     This script contains all the functions
#     related to one way test. These allow to do the test (ANOVA or KS), run
#     post hoc test, and plot the reults.
########################################


library(rstatix)
library(ggpubr)

# Test functions
## One-way anova test function
one_ANOVA <- function(d, treatment, variable, ANOVA_type = 2) {
  
  # Load vars
  local_d <- copy(d)
  local_d[, id := 1:.N]
  formula_obj <- paste(variable, "~", treatment) %>%
    formula(.)
  # Run ANOVA
  out <- local_d %>%
    anova_test(formula_obj, data = .,wid ="id",  detailed = T, type = ANOVA_type) %>% 
    get_anova_table(.,) %>%
    data.table
  out[, method := "One-Way-ANOVA"]
  
  return(out)
}

## Kuskall-Wallis test function
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

# Post hocs
## ANOVA post hoc
post_hoc_aov <- function(d, treatment, variable, p.adjust.method = "bonferroni") {
  
  # Load vars
  local_d <- copy(d)
  formula_obj <- paste(variable, "~", treatment) %>%
    formula(.)
  
  # Run test
  out <- aov(formula_obj, data = local_d) %>% 
    tukey_hsd(., p.adjust.method = ) %>% 
    data.table
  
  return(out)
}

# KS post hoc
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

# Combined test function to take input from user
one_way_test <- function(d, input) {
  
  if (input$one_test == "One-Way-ANOVA") {
    one_test <- one_ANOVA(d, treatment = input$treatment,variable =  input$variable, ANOVA_type = 2)
    one_post_hoc <- post_hoc_aov(d, treatment = input$treatment,variable =  input$variable)
    
  } else {
    one_test <- K_wallis(d, treatment = input$treatment, variable = input$variable)
    one_post_hoc <- post_hoc_kw(d, treatment = input$treatment, variable = input$variable)
    
  }
  
  out <- list(one_test, one_post_hoc) 
  return(out)
}

# Plot functionality
plot_one_way <- function(d, 
                         variable, 
                         treatment,
                         post_hoc, 
                         ref_group = NULL, 
                         plot_type = "barplot", 
                         col_palette = "jco") {
  
  position <- d[, .(pos_y = max(.SD) + max(.SD) * .25), 
             by = treatment, 
             .SDcols = variable]
  
  if (plot_type == "barplot") {
    ggpubr::ggbarplot(d, 
                      y = variable, 
                      x = treatment,
                      fill = treatment,
                      add = "mean_sd", 
                      palette = col_palette,
                      ) +
      stat_pvalue_manual(post_hoc, 
                         y.position = position$pos_y,
                         ref.group = ref_group, 
                         abel = "p.adj.signif") +
      theme_pubr()
    
  } else {
    ggpubr::ggboxplot(d, 
                      y = variable, 
                      x = treatment, 
                      fill = treatment,
                      palette = col_palette) +
      stat_pvalue_manual(post_hoc, 
                         y.position = position$pos_y,
                         ref.group = ref_group, 
                         abel = "p.adj.signif") +
      theme_pubr()
    
  }
    
  
}
