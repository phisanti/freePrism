#' reporter 
#'
#' @description A utils function
#'
#' @return The return value, if any, from executing the utility.
#' @import glue
#' @export
#' @noRd
reportr <- function(test, print_report = FALSE) {
  
  test_classes <- class(test)
  n <- nrow(test)
  if ("t_test" %in% test_classes& n == 1) {
    test_report <- reportr_ttest(test, print_report)
  } else if ("t_test" %in% test_classes & n > 1) {
    test_report <- reportr_mutiple_ttest(test, print_report) 
  } else if ("wilcox_test" %in% test_classes  & n == 1) {
    test_report <- reportr_wilcox(test, print_report) 
  } else if ("wilcox_test" %in% test_classes & n > 1) {
    test_report <- reportr_mutiple_wilcox(test, print_report) 
  } else if ("anova_test"  %in% test_classes) {
    test_report <- reportr_anova(test, print_report) 
  } else if ("kruskal_test"  %in% test_classes) {
    test_report <- reportr_kruskal(test, print_report) 
  } else if ("tukey_hsd" %in% test_classes) {
    test_report <- reportr_tukey_test(test, print_report) 
  } else if ("dunn_test" %in% test_classes) {
    test_report <- reportr_dunn_test(test, print_report) 
  }
  
  return(test_report)
}

#' reporter 
#'
#' @description A utils function
#'
#' @return The return value, if any, from executing the utility.
#' @importFrom magrittr not
#' @import glue
#' @export
#' @noRd
reportr_ttest <- function(means_test, print_report = FALSE) {
  
  # Load variables
  test_attributes <- attr(means_test, 'args')
  test_method <- test_attributes$method
  is_paired <- test_attributes$paired
  formula_str <- formula_to_str(test_attributes$formula)
  variable <- formula_str$lhs
  treatment <- formula_str$rhs
  pval <- means_test$p %>% signif(., digits=3)
  group1 <- means_test$group1
  group2 <- means_test$group2
  estimate <- means_test$estimate %>% signif(., digits=3)
  cilow <- means_test$conf.low %>% signif(., digits=3)
  cihigh <- means_test$conf.high %>% signif(., digits=3)
  
  # Select test
  if (test_method %in% c("T-test", "t_test") & not(test_attributes$var.equal)) {
    test_method <- "Welch Two sample T-test"
  } else if (test_method %in% c("T-test", "t_test")) {
    test_method <- "Two sample T-test"
  }
  
  if (is_paired) {
    test_method <- glue("Paired {test_method}")
    means_text <- ""
  } else {
    estimate1 <- means_test$estimate1 %>% signif(., digits=3)
    estimate2 <- means_test$estimate2 %>% signif(., digits=3)
    means_text <- glue("({group1} = {estimate1}, and {group2} = {estimate2})")
  }
  
  # Verbalise p-val
  pval_sig <- text_pval(pval)
  
  title <- glue(test_method)
  top <- "Effect sizes were labelled following Cohen's (1988) recommendations."
  text <- glue("The {test_method} testing the difference of {variable} \\
                by {treatment} suggest that the difference is {pval_sig}. Furthermore, \\
               the difference in of the means {means_text}, is {estimate}, \\
               95% CI [{cilow}, {cihigh}].")
  
  out_text <- c("title" = title,"top" = top, "text" = text)
  
  if (print_report) {
    print(paste(out_text, collapse = "\n\n") %>% cat)
  }
  
  return(out_text)
}

#' reporter 
#'
#' @description A utils function
#'
#' @return The return value, if any, from executing the utility.
#' @import glue
#' @noRd
#' @export
reportr_wilcox <- function(wilcox_test, print_report = FALSE) {
  
  # Load variables
  test_attributes <- attr(wilcox_test, 'args')
  formula_str <- formula_to_str(test_attributes$formula)
  exact <- ifelse(is.null(test_attributes$exact), FALSE, test_attributes$exact)
  variable <- formula_str$lhs
  treatment <- formula_str$rhs
  method <- test_attributes$method
  print(method)
  group1 <- wilcox_test$group1
  group2 <- wilcox_test$group2
  pval <- wilcox_test$p %>% signif(., digits=3)
  wstat <- wilcox_test$statistic %>% signif(., digits=3)
  
  # Select test
  if (method %in% c("Wilcoxon", "wilcox_test")) {
    if (exact) {
      method <- "Exact Wilcoxon rank sum test"
    } else if (not(is.null(test_attributes$continuity))) {
      method <- "Wilcoxon rank sum test with continuity correction"
    } else {
      method <- "Wilcoxon rank sum test"
    }
  }
  
  # Verbalise p-val
  pval_sig <- text_pval(pval)
  
  
  # Verbalise effect size
  title <- glue("{method} for {variable}")
  top <- " " 
  text <- glue("The {method} testing the difference of {variable} by {treatment} \\
                suggest that the difference between {group1} and {group2} is {pval_sig}.")
  
  out_text <- c("title" = title,"top" = top, "text" = text)
  if (print_report) {
    print(paste(out_text, collapse = "\n\n") %>% cat)
  }
  return(out_text)
}

#' reporter 
#'
#' @description A utils function
#'
#' @return The return value, if any, from executing the utility.
#' @import glue
#' @noRd
#' @export
reportr_anova <- function(anova_test, print_report = FALSE) {
  
  # Load variables
  test_attributes <- attr(anova_test, "args")
  formula_str <- formula_to_str(test_attributes$formula)
  variable <- formula_str$lhs
  treatment <- formula_str$rhs
  covariate <- ifelse(!is.null(test_attributes$covariate), test_attributes$covariate, "")
  white_adjust <- test_attributes$white.adjust
  method <- test_attributes$method
  type <- test_attributes$type
  
  # Get full method
  anova_types <- c("I", "II", "III")
  anova_type <- anova_types[type]
  if (length(treatment) == 1) {
    method <- glue("One-way ANOVA Type {anova_type}")
  } else if (length(treatment) > 1 & grepl(pattern = "ranked", x = variable)) {
    method <- glue("Ranked Two-way ANOVA Type {anova_type}")
    
  } else if (length(treatment) > 1) {
    method <- glue("Two-way ANOVA Type {anova_type}")
  }
  
  title <- glue("{method} for {variable}")
  top0 <- "ANOVA (Analysis of Variance) is a statistical method used to test 
  whether the means of two or more groups are significantly different from each 
  other. It assumes independence of observations, normal distribution within each
  group and Homogeneity of variances across groups"
  
  if (anova_type == "I") {
    top1 <- glue("ANOVA Type {anova_type} assumes balanced designs and it is used
    when the variables are categorical and the main effects are of primary interest.")
  } else if (anova_type == "II") {
    top1 <- glue("ANOVA Type {anova_type} accepts unbalanced designs and 
                 it is used when the variables are continuous and the 
                 interactions are of primary interest.")
  } else if (anova_type == "II") {
    top1 <- glue("ANOVA Type {anova_type} aims to resolve the effects that are 
    orthogonal to the others. That is,  it isolates the unique variance associated 
    with each variable or interaction, while controlling for the effects of other 
    variables. It is used when the variables are categorical and the interactions 
                 are of primary interest.")
  }

  # Verbalise Effect
  n <- nrow(anova_test)
  text_collection <- vector("character", n)
  for (row in 1:n) {
    effect <- anova_test[row, "Effect"]
    Fstat <- anova_test[row, "F"]
    pval <- anova_test[row, "p"]
    ges <- anova_test[row, "ges"]
    eff_type <- "main effect"
    effsize_dir <- ifelse(ges < 0, "negative", "positive")
    effsize_desc <- glue("{effsize_dir} effect (generalized eta squared = {abs(anova_test$ges)})")
    
    pval_sig <- text_pval(pval)
    if (grepl(pattern = ":", x = effect)) {
      eff_type <- "interaction"
    }
    eff_text <- glue("- The {eff_type} {effect} is {pval_sig} and has a {effsize_dir} impact on {variable} (generalized eta^2 = {abs(ges)})\n")
    text_collection[row] <- eff_text
  }
  
  if (white_adjust) {
    top2 <- glue("ANOVA test for {variable} with White's adjustment for heteroscedasticity. The result for each effect is: \n")
  } else {
    top2 <- glue("ANOVA test for {variable}. The result for each effect is: \n")
  }
  
  top <- glue_collapse(c(top0, top1, top2), sep = "\n")
  text <- text_collection
  out_text <- c("title" = title, top = top, text)
  
  if (print_report) {
    print(glue_collapse(text, sep = "\n") %>%
            glue_collapse(c(title, top, .), sep = "\n\n")  %>% cat)
  }
  
  return(out_text)
}

#' reporter 
#'
#' @description A utils function
#'
#' @return The return value, if any, from executing the utility.
#' @import glue
#' @noRd
#' @export
reportr_kruskal <- function(kruskal_test, print_report = FALSE) {
  
  # Load variables
  test_attributes <- attr(kruskal_test, 'args')
  forumla_str <- formula_to_str(test_attributes$formula)
  variable <- formula_str$lhs
  treatment <- formula_str$rhs
  n <- kruskal_test[1, "n"]
  stat <- kruskal_test[1, "statistic"]
  df <- kruskal_test[1, "df"]
  p <- kruskal_test[1, "p"]
  method <- kruskal_test["method"]
  
  pval_sign <- text_pval(p)
  # Construct text
  title <- glue("Kruskal-Wallis Test for {variable} by {treatment}")
  top <- glue("The Kruskal-Wallis test is a non-parametric test used to \\
              determine whether there is a statistically significant difference \\
              between the medians of three or more independent groups.")
  text <- glue("The Kruskal-Wallis test is used to compare the median ranks of \\ 
  {variable} across the {treatment}. The {treatment} effect was found to be \\ 
               {pval_sign}")
  
  out_text <- c("title" = title, "top" = top, "text" = text)
  
  if (print_report) {
    print(glue_collapse(out_text, sep = "\n\n"))
  }
  
  return(out_text)
}

#' reporter 
#'
#' @description A utils function
#'
#' @return The return value, if any, from executing the utility.
#' @import glue
#' @noRd
#' @export
reportr_mutiple_ttest <- function(means_test, print_report = FALSE) {
  
  # Load variables
  test_attributes <- attr(means_test, 'args')
  test_method <- test_attributes$method
  is_paired <- test_attributes$paired
  pval_adj_method <- test_attributes$p.adjust.method
  formula_str <- formula_to_str(test_attributes$formula)
  variable <- formula_str$lhs
  treatment <- formula_str$rhs
  
  
  # Select test
  if (test_method %in% c("T-test", "t_test") & not(test_attributes$var.equal)) {
    test_method <- "Welch Two sample T-test"
  } else if (test_method %in% c("T-test", "t_test")) {
    test_method <- "Two sample T-test"
  }
  
  if (is_paired) {
    test_method <- glue("paired {test_method}")
  }
  
  # Generate title and top text
  title <- glue("Two samples Multiple ", test_method)
  top <- glue("Effect sizes were labelled following Cohen's (1988) recommendations. \\
  The p-value was corrected using {pval_adj_method} method.")
  
  # Iterate over tests
  n <- nrow(means_test)
  tests_text <- vector("character", n)
  for (i in seq_len(n)) {
    
    # Load contrast-i variables
    pval <- means_test[i, "p.adj"] %>% signif(., digits=3)
    group1 <- means_test[i, "group1"]
    group2 <- means_test[i, "group2"]
    estimate1 <- means_test[i, "estimate1"] %>% signif(., digits=3)
    estimate2 <- means_test[i, "estimate2"] %>% signif(., digits=3)
    estimate <- means_test[i, "estimate"] %>% signif(., digits=3)
    cilow <- means_test[i, "conf.low"] %>% signif(., digits=3)
    cihigh <- means_test[i, "conf.high"] %>% signif(., digits=3)
    
    # Verbalise p-val
    pval_sig <- text_pval(pval)
    tests_text[i] <- glue(" - The {test_method} testing the difference of {variable} \\
                by {treatment} suggest that the difference is {pval_sig}. Furthermore, \\
               the difference in of the means ({group1} = {estimate1}, and {group2} = {estimate2}), \\
               is {estimate}, 95% CI [{cilow}, {cihigh}]")
    
    
    
  }
  out_text <- c("title" = title,"top" = top, "text" = tests_text)
  
  if (print_report) {
    print(paste(out_text, collapse = "\n\n") %>% cat)
  }
  
  return(out_text)
}

#' reporter 
#'
#' @description A utils function
#'
#' @return The return value, if any, from executing the utility.
#' @import glue
#' @noRd
#' @export
reportr_mutiple_wilcox <- function(wilcox_test, print_report = FALSE) {
  
  # Load variables
  test_attributes <- attr(wilcox_test, 'args')
  formula_str <- formula_to_str(test_attributes$formula)
  method <- test_attributes$method
  pval_adj_method <- test_attributes$p.adjust.method
  exact <- ifelse(is.null(test_attributes$exact), FALSE, test_attributes$exact)
  variable <- formula_str$lhs
  treatment <- formula_str$rhs
  
  # Select test
  if (method %in% c("Wilcoxon", "wilcox_test")) {
    if (exact) {
      method <- "Exact Wilcoxon rank sum test"
    } else if (not(is.null(test_attributes$continuity))) {
      method <- "Wilcoxon rank sum test with continuity correction"
    } else {
      method <- "Wilcoxon rank sum test"
    }
  }
  
  # Generate title and top text
  title <- glue("Two samples Multiple ", method)
  top <- glue("The p-value was corrected using {pval_adj_method} method.")
  
  # Iterate over tests
  n <- nrow(wilcox_test)
  tests_text <- vector("character", n)
  for (i in seq_len(n)) {
    
    # Load contrast-i variables
    group1 <- wilcox_test$group1[i]
    group2 <- wilcox_test$group2[i]
    pval <- wilcox_test$p.adj[i] %>% signif(., digits=3)
    wstat <- wilcox_test$statistic[i] %>% signif(., digits=3)
    
    # Verbalise p-val
    pval_sig <- text_pval(pval)
    tests_text[i] <- glue("- The {method} testing the difference of {variable} by {treatment} \\
                suggest that the difference between {group1} and {group2} is {pval_sig}.")
    
  }
  out_text <- c("title" = title,"top" = top, "text" = tests_text)
  
  if (print_report) {
    print(paste(out_text, collapse = "\n\n") %>% cat)
  }
  
  return(out_text)
}

#' reporter 
#'
#' @description A utils function
#'
#' @return The return value, if any, from executing the utility.
#' @import glue
#' @noRd
reportr_dunn_test <- function(dunn_test, print_report = FALSE) {
  
  
  # Load variables
  test_attributes <- attr(dunn_test, 'args')
  test_method <- test_attributes$method
  is_paired <- test_attributes$paired
  pval_adj_method <- test_attributes$p.adjust.method
  formula_str <- formula_to_str(test_attributes$formula)
  variable <- formula_str$lhs
  treatment <- formula_str$rhs
  
  
  # Generate title and top text
  title <- glue("Two samples Multiple Dunn Test")
  top <- glue("The Dunn test is a non-parametric test that  uses the rank sums \\
  of the observations to make pairwise comparisons. \\
  The p-value was corrected using {pval_adj_method} method.")
  
  # Iterate over tests
  n <- nrow(dunn_test)
  tests_text <- vector("character", n)
  for (i in seq_len(n)) {
    
    # Load contrast-i variables
    pval <- dunn_test[i, "p.adj"] %>% signif(., digits=3)
    group1 <- dunn_test[i, "group1"]
    group2 <- dunn_test[i, "group2"]
    estimate1 <- dunn_test[i, "estimate1"] %>% signif(., digits=3)
    estimate2 <- dunn_test[i, "estimate2"] %>% signif(., digits=3)
    estimate <- dunn_test[i, "estimate"] %>% signif(., digits=3)
    
    # Verbalise p-val
    pval_sig <- text_pval(pval)
    tests_text[i] <- glue(" - The {test_method} testing the difference of {variable} \\
                by {treatment} suggest that the difference is {pval_sig}. Furthermore, \\
               the difference in of the means ({group1} = {estimate1}, and {group2} = {estimate2}), \\
               is {estimate}.")
  }
  out_text <- c("title" = title,"top" = top, "text" = tests_text)
  
  if (print_report) {
    print(paste(out_text, collapse = "\n\n") %>% cat)
  }
  
  return(out_text)
}
#' reporter 
#'
#' @description A utils function
#'
#' @return The return value, if any, from executing the utility.
#' @import glue
#' @noRd
reportr_tukey_test <- function(tukey_test, print_report = FALSE) {
  
  
  # Load variables
  test_attributes <- attr(tukey_test, 'args')
  test_method <- test_attributes$method
  is_paired <- test_attributes$paired
  pval_adj_method <- test_attributes$p.adjust.method
  formula_str <- formula_to_str(test_attributes$formula)
  variable <- formula_str$lhs
  treatment <- formula_str$rhs
  
  # Rename test
  if(test_method == "tukey_hsd") {
    test_method <- "Tukey HSD Test"
  }
  
  # If multiple treatment, then join
  if (length(treatment) == 1) {
    treatment_str <- treatment
  } else if (length(treatment) > 1) {
    treatment_str <- glue_collapse(treatment, sep = ",")
  }
  
  # Generate title and top text
  title <- glue("Tukey Honest Significant Differences Test")
  top <- glue("The Tukey HSD test is a parametric posthoc test that compares the 
  means of every treatment to the means of every other treatment; that is, it 
  applies simultaneously to the set of all pairwise comparisons. Then, it 
  identifies any difference between two means that is greater than the 
  expected standard error.
  This {test_method} test measures the pairwise effect of {treatment_str} on 
  {variable}. The p-value was corrected using {pval_adj_method} method.")
  
  # Iterate over tests
  n <- nrow(tukey_test)
  tests_text <- vector("character", n)
  for (i in seq_len(n)) {
    
    # Load contrast-i variables
    pval <- tukey_test[i, "p.adj"] %>% signif(., digits=3)
    group1 <- tukey_test[i, "group1"]
    group2 <- tukey_test[i, "group2"]
    estimate <- tukey_test[i, "estimate"] %>% signif(., digits=3)
    cilow <- tukey_test[i, "conf.low"] %>% signif(., digits=3)
    cihigh <- tukey_test[i, "conf.high"] %>% signif(., digits=3)
    
    # Verbalise p-val
    pval_sig <- text_pval(pval)
    tests_text[i] <- glue(" - The {test_method} testing the difference of {variable} \\
                between {group1} and {group2} suggest that the difference is {pval_sig}. The 
                difference in of the means is {estimate}, 95% CI [{cilow}, {cihigh}].")
  }
  out_text <- c("title" = title,"top" = top, "text" = tests_text)
  
  if (print_report) {
    
    print(paste(out_text, collapse = "\n\n") %>% cat)
  }
  
  return(out_text)
}

#' reporter 
#'
#' @description A utils function
#'
#' @return The return value, if any, from executing the utility.
#' @import glue
#' @noRd

text_pval <- function(pval, cutoff = 0.05) {
  pval_tex <- ifelse(pval < cutoff, 
                     "statistically significant (p-value = pval)", 
                     "statistically not significant (p-value = pval)")
  return(pval_tex)
}
#' reporter 
#'
#' @description A utils function
#'
#' @return The return value, if any, from executing the utility.
#' @import glue
#' @noRd

text_pval <- function(pval, cutoff = 0.05) {
  pval_tex <- ifelse(pval < cutoff, 
                     glue("statistically significant (p-value = {pval})"), 
                          glue("statistically not significant (p-value = {pval})")
  )
  return(pval_tex)
}

