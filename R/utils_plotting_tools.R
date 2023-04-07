#' Plot_lm
#'
#' @description A utils function
#'
#' @return The return value, if any, from executing the utility.
#'
#' @noRd
#' @export
mean_sd <- ggpubr::mean_sd

#' Plot_lm
#'
#' @description A utils function
#'
#' @return The return value, if any, from executing the utility.
#'
#' @noRd

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

#' Plot_lm
#'
#' @description A utils function
#'
#' @return The return value, if any, from executing the utility.
#' @import ggfortify
#' @noRd
ggplot_lm <- function(d, model, input) {
  
  # Shape data
  local_d <- copy(d)
  xvar <- input$xvar
  yvar <- input$yvar
  plot_type <- input$plot_type
  
  if ("colvar" %in% names(input) && not(input$colvar == "")) {
    
    colvar <- input$colvar
    local_d[, c("xvar", "yvar", "colvar") := .SD, 
            .SDcols = c(xvar, yvar, colvar)]
    col_var_present <- T
  } else  {
    
    local_d[, c("xvar", "yvar") := .SD, 
            .SDcols = c(xvar, yvar)]
    local_d[, colvar := as.factor(0)]
    col_var_present <- F
  }
  
  coef_table <- summary(model) %>%
    coefficients %>%
    data.table(., keep.rownames = T)
  setnames(coef_table, c("variable", "estimate", "error", "t_val", "p_val"))
  coef_table[, p_val_label := signif(p_val, 3)]
  
  # Select Plot
  
  if (plot_type == "model check") {
    ggp <- autoplot(model, which = 1:6, ncol = 3, 
                    label.size = 3, data = local_d,
             colour = 'colvar')
  } else if (plot_type == "linear") {
    
    local_d[, pred_vals := fitted(model)]
    ggp <- ggplot(local_d, 
                   aes(x = xvar)) +
      geom_point(aes(y = yvar, col = colvar), show.legend = col_var_present) +
      geom_point(aes(y = pred_vals), col = "firebrick")
    
  }

  return(ggp)
}

#' Plot_one_way
#'
#' @description A utils function
#'
#' @return The return value, if any, from executing the utility.
#' @importFrom ggpubr mean_sd
#' @noRd
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

#' plotting_tools 
#'
#' @description A utils function
#'
#' @return The return value, if any, from executing the utility.
#' @import ggpubr
#' @importFrom ggpubr mean_sd mean_ci ggboxplot ggbarplot gghistogram
#' @importFrom ggplot2 mean_se
#' @noRd
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
  ggp <- ggp +
    theme_pubr()
  
  return(ggp)
}

#' plotting_tools 
#'
#' @description A utils function
#'
#' @return The return value, if any, from executing the utility.
#' 
#' @noRd
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
   
    ggp <- gg_corplot(d, treatment = treatment) 
  }
  
  return(ggp)
}

#' plotting_tools 
#'
#' @description A utils function
#'
#' @return The return value, if any, from executing the utility.
#' 
#' @noRd

ggplot_hist <- function(d, treatment, colpal = "jco") {
  
  # Load vars
  local_d <- copy(d)
  num_cols <- d[, .SD, .SDcols = is.numeric] %>% 
    colnames()

  # Calculate nbins
  
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

gg_corplot <- function(d, treatment) {
  
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

#' Add pvalues

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
