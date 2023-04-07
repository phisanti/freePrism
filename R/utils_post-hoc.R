#' Conducts post-hoc pairwise comparisons after an ANOVA test.
#'
#' This function conducts post-hoc pairwise comparisons between groups in a 
#' dataset, after an ANOVA test has indicated significant differences among the 
#' groups. The function uses the Tukey's Honestly Significant Difference (HSD) 
#' test to compare all possible pairwise differences between the groups.
#'
#' @param d A data frame containing the data to be analyzed.
#' @param treatment A string specifying the name of the categorical variable that defines the groups to be compared.
#' @param variable A string specifying the name of the continuous variable to be analyzed.
#' @param p.adjust.method A string specifying the method used to adjust p-values for multiple comparisons. Default is "bonferroni".
#'
#' @return A data table containing the results of the pairwise comparisons, including the names of the compared groups, the difference in means, the standard error, the t-value, the degrees of freedom, the p-value, and any adjustment for multiple testing.
#'
#' @importFrom data.table data.table
#' @importFrom magrittr %>%
#'
#' @export
posthoc_test <- function(d, treatment, variable, test, p.adjust.method = "bonferroni") {
  
  # Load vars
  local_d <- copy(d)
  formula_obj <- paste(variable, "~", treatment) %>%
    formula(.)
  local_d[, (treatment) := lapply(.SD, as.factor),
          .SDcols = treatment]
  
  # Run test
  if (test == "Tukey HSD") {
    out <- tukey_hsd(x = local_d, formula = formula_obj, 
                     p.adjust.method = p.adjust.method)
    
  } else if (test == "Dunn Test") {
    out <- local_d %>%
      dunn_test(., formula_obj, 
                p.adjust.method = p.adjust.method, 
                detailed = TRUE)
    
  } else if (test == "Mult. T-test") {
    out <- local_d %>%
      t_test(., formula_obj, 
             p.adjust.method = p.adjust.method,
             detailed = TRUE)
    
  } else if (test == "Mult. Wilcox") {
    
    out <- local_d %>%
      wilcox_test(., formula_obj, 
                  p.adjust.method = p.adjust.method,
                  detailed = TRUE)
  }
  
  out_report <- reportr(out)

  # Add test attributes
  attr_out <- attr(out, "args")
  out <-  data.table(out)
  setattr(out, "args", attr_out) 
  out[, method := test]
  return(list(posthoc = out, posthoc_report = out_report))
}

#' Conducts post-hoc pairwise comparisons after an ANOVA test.
#'
#' This function conducts post-hoc pairwise comparisons between groups in a 
#' dataset, after an ANOVA test has indicated significant differences among the 
#' groups. The function uses the Tukey's Honestly Significant Difference (HSD) 
#' test to compare all possible pairwise differences between the groups.
#'
#' @param d A data frame containing the data to be analyzed.
#' @param treatment A string specifying the name of the categorical variable that defines the groups to be compared.
#' @param variable A string specifying the name of the continuous variable to be analyzed.
#' @param p.adjust.method A string specifying the method used to adjust p-values for multiple comparisons. Default is "bonferroni".
#'
#' @return A data table containing the results of the pairwise comparisons, including the names of the compared groups, the difference in means, the standard error, the t-value, the degrees of freedom, the p-value, and any adjustment for multiple testing.
#'
#' @importFrom data.table data.table
#' @importFrom magrittr %>%
#'
#' @export
ph_tukey <- function(d, treatment, variable, p.adjust.method = "bonferroni") {
  
  # Load vars
  local_d <- copy(d)
  formula_obj <- paste(variable, "~", treatment) %>%
    formula(.)
  local_d[, (treatment) := lapply(.SD, as.factor),
          .SDcols = treatment]
  # Run test
  out <- tukey_hsd(x = local_d, formula = formula_obj, 
                   p.adjust.method = p.adjust.method
                   )
    data.table
  return(out)
}
#' Conducts post-hoc pairwise comparisons after a Kruskal-Wallis test.
#'
#' This function conducts post-hoc pairwise comparisons between groups in a 
#' dataset, after a Kruskal-Wallis test has indicated significant differences 
#' among the groups. The function uses the Dunn's test to compare all possible 
#' pairwise differences between the groups.
#'
#' @param d A data frame containing the data to be analyzed.
#' @param treatment A string specifying the name of the categorical variable that defines the groups to be compared.
#' @param variable A string specifying the name of the continuous variable to be analyzed.
#' @param p.adjust.method A string specifying the method used to adjust p-values for multiple comparisons. Default is "bonferroni".
#'
#' @return A data table containing the results of the pairwise comparisons, including the names of the compared groups, the test statistic, the p-value, and any adjustment for multiple testing.
#'
#' @examples
#' # Load example data
#' data(iris)
#'
#' # Conduct Kruskal-Wallis test
#' kruskal.test(Sepal.Length ~ Species, data = iris) %>%
#'   print()
#'
#' # Conduct post-hoc pairwise comparisons using Dunn's test
#' post_hoc_kw(iris, "Species", "Sepal.Length", p.adjust.method = "fdr") %>%
#'   print()
#'
#' @importFrom data.table data.table
#' @importFrom rstatix dunn_test
#' @importFrom magrittr %>%
#'
#' @export
ph_dunn <- function(d, treatment, variable, p.adjust.method = "bonferroni") {
  
  # Load vars
  local_d <- copy(d)
  formula_obj <- paste(variable, "~", treatment) %>%
    formula(.)
  # Run Test
  
  return(out)
  
}

#' Conducts post-hoc pairwise comparisons t-test.
#'
#' This function conducts post-hoc pairwise comparisons between groups in a 
#' dataset. The function uses the Welch's t-test to compare all possible pairwise 
#' differences between the groups, and adjusts p-values for multiple comparisons 
#' using the specified method.
#'
#' @param d A data frame containing the data to be analyzed.
#' @param treatment A string specifying the name of the categorical variable that defines the groups to be compared.
#' @param variable A string specifying the name of the continuous variable to be analyzed.
#' @param p.adjust.method A string specifying the method used to adjust p-values for multiple comparisons. Default is "bonferroni".
#'
#' @return A data table containing the results of the pairwise comparisons, including the names of the compared groups, the difference in means, the standard error, the t-value, the degrees of freedom, the p-value, and any adjustment for multiple testing.
#'
#' @importFrom data.table data.table
#' @importFrom rstatix t_test
#' @importFrom magrittr %>%
#'
#' @export
ph_mult_ttest <- function(d, treatment, variable, p.adjust.method = "bonferroni") {
  
  # Load vars
  local_d <- copy(d)
  formula_obj <- paste(variable, "~", treatment) %>%
    formula(.)
  # Run Test
  
  return(out)
  
}

#' Conducts post-hoc pairwise comparisons Wilcox-test.
#'
#' This function conducts post-hoc pairwise comparisons between groups in a 
#' dataset. The function uses the Wilcox to compare all possible pairwise 
#' differences between the groups, and adjusts p-values for multiple comparisons 
#' using the specified method.
#'
#' @param d A data frame containing the data to be analyzed.
#' @param treatment A string specifying the name of the categorical variable that defines the groups to be compared.
#' @param variable A string specifying the name of the continuous variable to be analyzed.
#' @param p.adjust.method A string specifying the method used to adjust p-values for multiple comparisons. Default is "bonferroni".
#'
#' @return A data table containing the results of the pairwise comparisons, including the names of the compared groups, the difference in means, the standard error, the t-value, the degrees of freedom, the p-value, and any adjustment for multiple testing.
#'
#' @importFrom data.table data.table
#' @importFrom rstatix wilcox_test
#' @importFrom magrittr %>%
#'
#' @export
ph_mult_wilcox <- function(d, treatment, variable, p.adjust.method = "bonferroni") {
  
  # Load vars
  local_d <- copy(d)
  formula_obj <- paste(variable, "~", treatment) %>%
    formula(.)
  # Run Test
  out <- local_d %>%
    wilcox_test(., formula_obj, 
           p.adjust.method = p.adjust.method,
           detailed = TRUE) %>%
    data.table
  
  return(out)
  
}