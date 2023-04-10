#' Exporting ggpubr::mean_sd function
#'
#' The ggpubr package provides a function called mean_sd, which calculates the mean and standard deviation of a vector. However, in order to use this function within certain ggpubr functions, such as barplot, it is necessary to export it to a function within your own package. This function does not have any parameters or return values, it simply imports the ggpubr::mean_sd function and exports it to your package.
#'
#' @importFrom ggpubr mean_sd
#' @export
mean_sd <- ggpubr::mean_sd

#' Plot coefficients of a linear model
#'
#' This function plots the coefficients of a linear model with their corresponding standard errors and p-values.
#'
#' @param model The linear model object to be plotted.
#' @param input A list containing the input values for the function.
#'
#' @return A list containing the ggplot object generated.
#'
#' @importFrom ggplot2 ggplot geom_point geom_errorbarh geom_text geom_vline
#' @importFrom ggpubr theme_pubr
#' @export
plot_lmcoef <- function(model, input) {
  
  coef_table <- summary(model) %>%
    coefficients %>%
    data.table(., keep.rownames = T)
  setnames(coef_table, c("variable", "estimate", "error", "t_val", "p_val"))
  coef_table[, p_val_label := signif(p_val, 3)]
  
  ggp1 <- ggplot(coef_table, aes(x = estimate, y = variable)) +
    geom_point() +
    geom_errorbarh(aes(xmin = estimate - error, xmax = estimate + error), 
                   height = .25) +
    geom_text(aes(y = variable, label = p_val_label),
              vjust = -3) +
    geom_vline(xintercept = 0, linetype = 2, col = "firebrick") +
    theme_pubr()
  
  return(list(ggp1))
  
}

#' Plotting function for linear models
#'
#' This function takes in a data frame, a linear model, and several inputs to create a plot.
#' The input options are \code{xvar} for the independent variable, \code{yvar} for the dependent variable,
#' \code{colvar} for the grouping variable (if applicable), and \code{plot_type} for the type of plot
#' to be created ("model check" or "scatter").
#'
#' If \code{colvar} is not specified or is set to "none", the grouping variable is set to "colvar"
#' and all data points are colored the same. Otherwise, the grouping variable is set to the
#' specified \code{colvar}.
#'
#' For "model check" plots, the function creates a multiplot of six plots, showing the distribution
#' of residuals, normal Q-Q plot of residuals, scale-location plot, residuals vs. leverage plot,
#' cook's distance plot, and residual vs. fitted plot.
#'
#' For "scatter" plots, the function creates a scatterplot of the data with the regression line overlaid.
#' If a \code{colvar} is specified, the data points are colored by that variable. Additionally, the
#' function adds error bars to the plot representing the standard error of the prediction for each data point.
#'
#' @param d A data.frame
#' @param model A linear model object
#' @param input A list containing several options for creating the plot. The options are:
#' \describe{
#' \item{xvar}{The independent variable}
#' \item{yvar}{The dependent variable}
#' \item{colvar}{The grouping variable (optional)}
#' \item{plot_type}{The type of plot to be created ("model check" or "scatter")}
#' }
#'
#' @return A ggplot object
#' @import ggplot2
#' @import ggfortify
#' @importFrom ggpubr ggscatter
#' @importFrom ggpubr ggarrange
#' @importFrom data.table data.table
#' @export
ggplot_lm <- function(d, model, input) {
  
  # Shape data
  local_d <- copy(d)
  xvar <- input$xvar
  yvar <- input$yvar
  plot_type <- input$plot_type
  
  if (input$colvar == "" | input$colvar == "none") {
    colvar <- "colvar"
    local_d[, colvar := ""]
  } else  {
    colvar <- input$colvar

  }
  
  # Select Plot
  
  if (plot_type == "model check") {
    ggp_multiplot <- autoplot(model, which = 1:6, ncol = 3, 
                     label.size = 3, data = local_d, colour = 'colvar') + 
      theme_pubr(base_family = "Helvetica"
      )
    
    ggp <- ggarrange(plotlist = ggp_multiplot@plots)
  } else if (plot_type == "scatter") {
    local_d[, c("pred_vals", "se") := 
        predict(model, newdata = .SD, se.fit = T) %$% 
        .(fit, se.fit)]
    local_d[, c("upper", "lower") := .(pred_vals + se, pred_vals - se)]
    ggp <- ggpubr::ggscatter(local_d, x = xvar, yvar, color = colvar) +
      geom_line(aes_string(x = xvar, y = "pred_vals", col = colvar)) +
      geom_errorbar(aes_string(ymin = "lower", ymax = "upper"), width = 0.2)
    
    
  }
  ggp + 
     theme_pubr(base_family = "Helvetica"
                )
  return(ggp)
}

#' Plot ANOVA or Kruskal-Wallis test results with optional post-hoc analysis
#'
#' This function produces a plot of the results of an ANOVA or Kruskal-Wallis 
#' test, with optional post-hoc analysis. The function accepts a data frame 
#' \code{d} and an input list \code{input} that contains information on the test
#' to perform, the variables to use, and various plotting options. The function
#' also accepts an optional argument \code{post_hoc} which, if provided, is a 
#' list containing information on which post-hoc tests to perform and how to 
#' adjust for multiple comparisons. If no post-hoc analysis is desired, this 
#' argument can be left as the default value \code{NULL}.
#'
#' @param d A data frame containing the data to plot
#' @param input A list containing input options for the plot. See Details for more information.
#' @param post_hoc An optional list containing information on which post-hoc tests to perform and how to adjust for multiple comparisons.
#' @param ref.group An optional character value indicating the reference group for comparisons.
#'
#' @return A ggplot object containing the plot.
#' @export
plot_anova <- function(d, input,
                         post_hoc, 
                         ref.group = NULL) {
  # Load vars
  local_d <- copy(d)
  if (!is.null(input$test) && 
      input$test %in% c("One-Way-ANOVA", "Kruskall-Wallis")){
    xvar <- input$treatment
    yvar <- input$variable
    colvar <- input$treatment
    betweet_type <- "between"
    is_normal <- ifelse(input$test == "One-Way-ANOVA", TRUE, FALSE)
  } else {
    xvar <- input$xvar
    yvar <- input$yvar
    colvar <- input$colvar
    is_normal <- input$is_normal
    betweet_type <- input$betweet_type
  }
  plot_type <-  input$plot_type
  col_palette <-  input$colpal
  p_adjust_method <- input$p_adjust_method
  pval_format <- input$pval_format
  
  
  local_d[, xvar := lapply(.SD, as.factor), .SDcols = xvar]
  if (colvar == "none") {
    colvar <- "grey"
  }
  
  if (plot_type == "barplot") {
    ggp <- ggpubr::ggbarplot(local_d, 
                             y = yvar, 
                             x = xvar,
                             fill = colvar,
                             add = "mean_sd", 
                             position = position_dodge(0.8),
                             palette = col_palette,
    ) 
    
  } else {
    ggp <- ggpubr::ggboxplot(local_d, 
                             y = yvar, 
                             x = xvar, 
                             fill = colvar,
                             palette = col_palette) 
    
  }
  
  # Add p-values
  if (betweet_type == "between") {
    xvar <- colvar
    colvar <- xvar
  }
  if (is_normal) {
    test_type <- "t.test"
  } else {
    test_type <- "wilcox.test"
  }
  
  ggp <- add_pvals(ggp,
                   test_type = test_type,
                   xvar = xvar,
                   colvar = colvar,
                   pval_format = pval_format,
                   betweet_type = betweet_type,
                   p_adjust = p_adjust_method,
                   test_paired = FALSE,
                   dodge = 0.8)
  
  return(ggp)
}

#' Plot data for one-sample comparison test
#'
#' This function generates a plot to compare a single group to a reference value,
#' using the data and parameters provided.
#'
#' @param d A data.frame or data.table object containing the data to plot.
#' @param input A list object containing input parameters for the plot. Required 
#' parameters include: treatment, variable, ref.group, plot_type, and colpal. 
#' Optional parameters include: paired.
#' @param test_out A list object containing the results of a statistical test.
#' @return A ggplot object with the plotted data.
#' @import ggpubr
#' @importFrom data.table setorder
#' @importFrom ggpubr ggbarplot ggboxplot ggpaired gghistogram stat_pvalue_manual
#' theme_pubr
#' @examples
plot_one_comp_m <- function(d, 
                            input,
                            test_out) {
  
  # Load variables
  local_d <- copy(d)
  treatment <- input$treatment
  variable <- input$variable
  ref.group <- input$ref.group
  plot_type <- input$plot_type 
  col_palette <- input$colpal
  test_table <- data.table(test_out)
  
  if ("paired" %in% names(input)) {
    paired <- input$paired == TRUE
  } else {
    paired <- FALSE
  }
  # Get p-val label location
  local_d[,treatment := lapply(.SD, as.factor), .SDcols = treatment]
  position <- local_d[, .(pos_y = max(.SD) + max(.SD) * .25), 
                      by = treatment, 
                      .SDcols = variable]
  
  setorder(position, -pos_y)
  position <- position[1:nrow(test_out)]
  
  # Plot data
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
    
  } else if (plot_type == "histogram") {
    group_means <- local_d[, 
                           lapply(.SD, mean, na.rm = TRUE), 
                           .SDcols = variable,
                           by = treatment]
    ggp <- gghistogram(local_d, 
                       x = variable, 
                       fill = treatment, 
                       add = "mean" ,
                       palette = "jco")
    
  }
  
  if (plot_type %in% c("boxplot", "barplot")){
    ggp <-   ggp +
      ggpubr::stat_pvalue_manual(test_table,
                         y.position = position$pos_y,
                         # ref.group = ref.group,
                         label = "p") 
      
  }
  return(ggp)
}

#' Generate exploration plots
#'
#' This function generates exploration plots based on the input data and the selected plot type.
#'
#' @param d A data frame or data table containing the data to be plotted.
#' @param input A named list containing the input parameters for the plot. Required input parameters are:
#' \describe{
#' \item{plot_type}{A character string specifying the type of plot to be generated. Allowed values are "histogram", "QQ", and "Correation".}
#' \item{colpal}{A character string specifying the color palette to be used in the plot.}
#' \item{treatment}{A character string specifying the name of the treatment variable in the data.}
#' }
#'
#' @return A ggplot object containing the exploration plot.
ggplot_explore <- function(d, input) {
  
  # Load vars
  plot_type <- input$plot_type
  colpal <- input$colpal
  treatment <- input$treatment
  
  # Select plot
  if (plot_type == "histogram") {
    ggp <- ggplot_hist(d, treatment = treatment, colpal = colpal)
  } else if (plot_type == "QQ") {
    ggp <- ggplot_qq(d, treatment, colpal)
  } else if (plot_type == "Correation") {
   
    ggp <- gg_corplot(d) 
  }
  
  return(ggp)
}

#' Histogram plot
#'
#' This function takes a data table, a treatment variable and an optional color palette as inputs
#' and produces a histogram plot using ggplot2 with overlaid normal density and vertical lines
#' for the means of each numeric variable, grouped by the treatment variable.
#'
#' @param d a data table
#' @param treatment a character string representing the name of the treatment variable
#' @param colpal an optional character string representing the name of the color palette to use (default is "jco")
#'
#' @return a ggplot object
#' @export
ggplot_hist <- function(d, treatment, colpal = "jco") {
  
  # Load vars
  local_d <- copy(d)
  num_cols <- d[, .SD, .SDcols = is.numeric] %>% 
    colnames()

  # Reshape data
  if (treatment == "none") {
    
    local_d[, dummy_id := 1:.N]
    melt_d <- local_d[, c("dummy_id", num_cols), with = F] %>%
      melt.data.table(., id.vars = "dummy_id")
    treatment <- "black"
    colpal <- "black"
    varmeans <- melt_d[, .(varmean  = mean(value, na.rm = TRUE)), 
                       by = variable]
  } else {
    melt_d <- local_d[, c(treatment, num_cols), with = F] %>%
      melt.data.table(., id.vars = treatment)
    melt_d[, (treatment) := lapply(.SD, as.factor), .SDcols = treatment]
    varmeans <- melt_d[, .(varmean  = mean(value, na.rm = TRUE)), 
                       by = c("variable", treatment)]
    
  }
  
  # Plot data
  ggp <- gghistogram(melt_d, x = "value", y = "..density..", color  = treatment,
                     fill = treatment, rug = TRUE,
                     palette = colpal)  +
    facet_wrap(variable ~ ., scales = "free") 
    
  
  if (treatment %in% colnames(d)) {
    ggp <- ggp + 
      stat_overlay_normal_density(aes(y = ..density.., col = get(treatment)),
                                             linetype = "dashed") +
      geom_vline(data = varmeans, aes(xintercept = varmean, col = get(treatment)), 
                 linetype = "dashed")
    
  } else {
    ggp <- ggp + 
      stat_overlay_normal_density(aes(y = ..density..), 
                                  color = "red", linetype = "dashed")+
      geom_vline(data = varmeans, aes(xintercept = varmean), 
                 col = "black", linetype = "dashed")
  } 
  
  return(ggp)
}

#' Generate a correlation plot using gg_corplot
#'
#' This function generates a correlation plot for a given data frame, using the \code{corrplot} package.
#'
#' @param d A data frame to generate the plot for.
#' @return A corplot object
gg_corplot <- function(d) {
  
  local_d <- copy(d)
  M <- cor(local_d[, .SD, .SDcols = is.numeric])
  outplot <- corrplot::corrplot.mixed(M, upper = 'ellipse', order = 'hclust')
  
  return(outplot)
}

#' Create a QQ plot using ggplot
#'
#' @param d A data table containing the data to plot.
#' @param treatment A character string specifying the column name of the treatment variable in the data table.
#' If "none", a dummy treatment variable will be created and used for plotting.
#' @param colpal A character string specifying the color palette to use for plotting.
#' If treatment is "none", this parameter will be set to "black".
#'
#' @return A ggplot object representing a QQ plot of the data.
#'
#' @importFrom data.table copy
#' @importFrom ggpubr ggqqplot
#'
#' @examples
#' library(data.table)
#' d <- data.table(x = rnorm(100), y = rnorm(100), z = rnorm(100), treatment = rep(c("A", "B"), each = 50))
#' ggplot_qq(d, "treatment", "jco")
#'
#' @export
ggplot_qq <- function(d, treatment, colpal) {
  
  # Load vars
  local_d <- copy(d)
  num_cols <- d[, .SD, .SDcols = is.numeric] %>% 
    colnames()
  # Reshape data
  if (treatment == "none") {
    local_d[, dummy_id := 1:.N]
    melt_d <- local_d[, c("dummy_id", num_cols), with = F] %>%
      melt.data.table(., id.vars = "dummy_id")
    treatment <- "black"
    colpal <- "black"
  } else {
    melt_d <- local_d[, c(treatment, num_cols), with = F] %>%
      melt.data.table(., id.vars = treatment)
  }
  
  # Plot

  ggp <- ggpubr::ggqqplot(data = melt_d, 
                   x = "value", 
                   color = treatment, 
                   palette = colpal) +
      facet_wrap(variable ~ .,
                 scales = "free"
      )
  return(ggp)
}

#' Add p-values to a ggplot object
#'
#' This function adds p-values calculated from statistical tests to a ggplot 
#' object. The tests can be either t-tests, Wilcoxon rank sum tests, 
#' Kruskal-Wallis tests or Tukey's HSD tests. P-values can be displayed either 
#' as significance stars or as formatted numbers. Tests can be run either between
#'  or within categories.
#'
#' @param ggp a ggplot object.
#' @param test_type a character string indicating the type of test to be run. 
#' Valid options are "t.test" (default), "wilcox", "kwallis", and "tukey_hsd".
#' @param xvar a character string indicating the name of the x variable.
#' @param colvar a character string indicating the name of the variable to be 
#' used for color coding.
#' @param pval_format a character vector of length 2 indicating how p-values 
#' should be displayed. The first element indicates the format for 
#' non-significant p-values and the second element indicates the format for 
#' significant p-values. Valid options are "p.adj.format" (default) and "p.adj.signif".
#' @param betweet_type a character string indicating whether the test should 
#' be run between or within categories. Valid options are "within" (default) and "between".
#' @param p_adjust a character string indicating the method used for p-value 
#' adjustment. Valid options are any method accepted by the p.adjust function, 
#' such as "bonferroni", "holm", "BH", etc. Default is "bonferroni".
#' @param test_paired a logical value indicating whether the test should be run 
#' as a paired test or not. Default is FALSE.
#' @param dodge a numeric value indicating the amount of dodging to use for 
#' overlapping labels. Default is 0.8.
#' @return a ggplot object with added p-values.
#' @export
add_pvals <- function(ggp,
                      test_type = "t.test", 
                      xvar, 
                      colvar,
                      pval_format = c("p.adj.format", "p.adj.signif"),
                      betweet_type = "within",
                      p_adjust = "bonferroni", 
                      test_paired = FALSE,
                      dodge = 0.8) {
  
  # Select test
  if (test_type == "t.test") {
    test <- "t.test"
  } else if (test_type == "wilcox") {
    test <- "wilcox.test"
  } else if (test_type == "kwallis") {
    test <- "dunn_test"
  } else if (test_type == "tukey_hsd") {
    test <- "tukey_hsd"
  }
  
  # Select paired or not
  if (test_paired) {
    setpaired <- TRUE
  } else {
    setpaired <- NULL
  }
  # Select how to display p-value
  if (pval_format == "star") {
    pval_format <- "p.adj.signif"
  } else {
    pval_format <- "p.adj.format"
  }
  
  # Run test between categories or within
  if (betweet_type == "within") {
    ggp <- ggp +
      geom_pwc(
        aes_string(group = colvar),
        method = test, 
        method.args = list(p.adjust.method = p_adjust, paired = setpaired),  
        label = pval_format,
        bracket.nudge.y = -0.08)
    
  } else if (betweet_type == "between") {
    
    ggp <- ggp + geom_pwc(
      aes_string(group = xvar, color = xvar), 
      method = test, 
      method.args = list(p.adjust.method = p_adjust, paired = setpaired), 
      label = pval_format,
      p.adjust.by = "panel",
      group.by = "legend.var"
    )
    
  }
  
  return(ggp)
}
