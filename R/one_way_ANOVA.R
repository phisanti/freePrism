# The following scripts contains all the functions required to run one-way ANOVA
# and it's non-parametric equivalent.

# Test functions
## One-way anova test function
one_ANOVA <- function(d, treatment, variable, ANOVA_type = 2) {
  
  # Load vars
  . <- NULL
  local_d <- data.table::copy(d)
  local_d[, id := 1:.N]
  formula_obj <- paste(variable, "~", treatment) %>%
    formula
  # Run ANOVA
  out <- local_d %>%
    anova_test(formula_obj, data = .,wid ="id",  detailed = T, type = ANOVA_type) %>% 
    get_anova_table(.) %>%
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
    tukey_hsd(., p.adjust.method = p.adjust.method) %>% 
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
  
  # Load variables
  treatment <- input$treatment
  variable <- input$variable
  local_d <- copy(d)
  local_d[, (treatment) := lapply(.SD, as.factor), 
          .SDcols = treatment]
  
  if (input$one_test == "One-Way-ANOVA") {
    one_test <- one_ANOVA(local_d, treatment = treatment,variable =  variable, ANOVA_type = 2)
    one_post_hoc <- post_hoc_aov(local_d, treatment = treatment,variable =  variable)
    
  } else {
    one_test <- K_wallis(local_d, treatment = treatment, variable = variable)
    one_post_hoc <- post_hoc_kw(local_d, treatment = treatment, variable = variable)
    
  }
  
  out <- list(one_test, one_post_hoc) 
  return(out)
}

# Plot functionality
plot_one_way <- function(d, 
                         variable, 
                         treatment,
                         post_hoc, 
                         ref.group = NULL, 
                         plot_type, 
                         col_palette = "jco") {
  
  position <- d[, .(pos_y = max(.SD) + max(.SD) * .25), 
             by = treatment, 
             .SDcols = variable]
  setorder(position, -pos_y)
  position <- position[1:nrow(post_hoc)]
  
  if (plot_type == "barplot") {
    ggp <- ggpubr::ggbarplot(d, 
                      y = variable, 
                      x = treatment,
                      fill = treatment,
                      add = "mean_sd", 
                      palette = col_palette,
                      ) 
    
  } else {
    ggp <- ggpubr::ggboxplot(d, 
                      y = variable, 
                      x = treatment, 
                      fill = treatment,
                      palette = col_palette) 

  }
  ggp + stat_pvalue_manual(post_hoc, 
                                 y.position = position$pos_y,
                                 ref.group = ref.group, 
                                 label = "p.adj.signif") +
    theme_pubr()
    
  
}
#d <- data.table(iris)
#input <- list(treatment_aov = "Species", variable_aov = "Sepal.Length", ref.group = NULL, plot_type = "barplot", one_test = "One-Way-ANOVA")
#z <- one_way_test(d, input)
#plot_one_comp_m(d, input, test_out = z[[2]])

