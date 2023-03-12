
########################################
#   One-Way test
#     This script contains all the functions
#     related to one way test. These allow to do the test (ANOVA or KS), run
#     post hoc test, and plot the reults.
########################################


#library(rstatix)
#library(ggpubr)

# Test functions
## T-test function
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

## Wilcox-Mann-Witney-U test function
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

# Compare means, select test

comp_means_test <- function(d, input){
  
  treatment <- input$treatment_means
  variable <- input$variable
  paired <- input$paired
  ref.group = input$ref.group
  alt_h = input$alt_h
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

  
# Plot functionality
plot_one_comp_m <- function(d, 
                         input,
                         test_out) {
  
  # Load variables
  local_d <- copy(d)
  treatment <- input$treatment_means
  variable <- input$variable
  ref.group = input$ref.group
  plot_type = input$plot_type 
  col_palette = "jco"
  if ("paired" %in% names(input)) {
    paired <- input$paired == TRUE
  } else {
    paired <- FALSE
    }

    position <- local_d[, .(pos_y = max(.SD) + max(.SD) * .25), 
                by = treatment, 
                .SDcols = variable]
    setorder(position, -pos_y)
    position <- position[1:nrow(test_out)]
    
  if (plot_type == "barplot") {
    ggp <- ggpubr::ggbarplot(local_d, 
                      y = variable, 
                      x = treatment,
                      fill = treatment,
                      add = "mean_sd", 
                      palette = col_palette,
    )     
  }  else if (plot_type == "boxplot" & paired == TRUE) {
    ggp <- ggpaired(local_d, x = treatment, y = variable, color = treatment, palette = "jco", 
      line.color = "gray", line.size = 0.4) 
    
  } else if (plot_type == "boxplot") {
    ggp <- ggboxplot(local_d, x = treatment, y = variable, color = treatment, palette = "jco")
    
  }
    ggp +
      stat_pvalue_manual(test_out, 
                         y.position = position$pos_y,
                        # ref.group = ref.group, 
                         abel = "p") +
      theme_pubr()
    
  
  
}
#input <- list(treatment = "Species", mean_test = "Wilcox-test", paired = F, variable = "Sepal.Length", ref.group = NULL, plot_type = "barplot")
#z <- comp_means_test(d, input)
#plot_one_comp_m(d, input, test_out = z)

