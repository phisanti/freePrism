#' Plot_lm
#'
#' @description A utils function
#'
#' @return The return value, if any, from executing the utility.
#'
#' @noRd

ggplot_lm <- function(d, model, input) {
  
  # Shape data
  local_d <- copy(d)
  xvar <- input$xvar
  yvar <- input$yvar
  
  if ("colvar" %in% names(input) && input$colvar != "") {
    
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
  
  # make plot effects
  ggp1 <- ggplot(coef_table, aes(x = estimate, y = variable)) +
    geom_point() +
    geom_errorbarh(aes(xmin = estimate - error, xmax = estimate + error), 
                   height = .25) +
    geom_text(aes(y = variable, label = p_val_label),
              vjust = -3) +
    geom_vline(xintercept = 0, linetype = 2, col = "firebrick") +
    theme_pubr()
  
  # Plot 
  local_d[, pred_vals := fitted(model)]
  ggp2 <- ggplot(local_d, 
                 aes(x = xvar)) +
    geom_point(aes(y = yvar, col = colvar), show.legend = col_var_present) +
    geom_point(aes(y = pred_vals), col = "firebrick")
  
  
  return(list(ggp1 = ggp1, ggp2 = ggp2))
}
#' Plot_one_way
#'
#' @description A utils function
#'
#' @return The return value, if any, from executing the utility.
#'
#' @noRd
plot_two_way <- function(d, 
                         xval,
                         yval, 
                         colval,
                         post_hoc, 
                         ref.group = NULL, 
                         plot_type, 
                         col_palette = "jco") {
  
  local_d <- copy(d)
  local_d[, xval := lapply(.SD, as.factor), .SDcols = xval]
  if (colval == "") {
    colval <- "grey"
  }
  print(yval)
  print(yval)
  print(post_hoc)
  print(plot_type)
  if (plot_type == "barplot") {
    ggp <- ggpubr::ggbarplot(local_d, 
                             y = yval, 
                             x = xval,
                             fill = colval,
                             add = "mean_sd", 
                             palette = col_palette,
    ) 
    
  } else {
    ggp <- ggpubr::ggboxplot(local_d, 
                             y = yval, 
                             x = xval, 
                             fill = colval,
                             palette = col_palette) 
    
  }
  
  return(ggp)
}

#' Plot_one_way
#'
#' @description A utils function
#'
#' @return The return value, if any, from executing the utility.
#'
#' @noRd
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

#' plotting_tools 
#'
#' @description A utils function
#'
#' @return The return value, if any, from executing the utility.
#'
#' @noRd
plot_one_comp_m <- function(d, 
                            input,
                            test_out) {
  
  # Load variables
  local_d <- copy(d)
  treatment <- input$treatment
  variable <- input$variable
  ref.group = input$ref.group
  plot_type = input$plot_type 
  print(plot_type)
  col_palette = "jco"
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
      stat_pvalue_manual(test_out,
                         y.position = position$pos_y,
                         # ref.group = ref.group,
                         label = "p") 
      
  }
  ggp +
    theme_pubr()
}

#' plotting_tools 
#'
#' @description A utils function
#'
#' @return The return value, if any, from executing the utility.
#'
#' @noRd

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
      facet_wrap(variable ~ ., scales = "free") 
  } else if (group_is_correct) {
    ggplot(melt_d, aes(x = value)) +
      geom_histogram(aes(fill = get(group))) +
      facet_wrap(variable, scales = "free") 
    
  } else {
    
  }
}

#' QQ-plot 
#'
#' @description we plot the qqplot of each variable. 
#'
#' @return The return value, if any, from executing the utility.
#'
#' @noRd

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
    ) 
}