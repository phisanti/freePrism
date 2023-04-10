#' general 
#'
#' @description A utils function
#'
#' @return The return value, if any, from executing the utility.
#'
#' @noRd
read_data <- function(input) {
  
  if (input$data_source == "demo-iris") {
    
    d <- data.table(iris)
  } else if (input$data_source == "demo-tooth") {
    
    d <- data.table(ToothGrowth)
  } else if (is.null(input$df_upload_file)) {
    Sys.sleep(20)
  } else {
    
    d <- fread(input$df_upload_file$datapath)
  }
}
#' general 
#'
#' @description A utils function
#'
#' @return The return value, if any, from executing the utility.
#'
#' @noRd

round_siginf_table <- function(d) {
  
  local_d <- as.data.table(d) %>%
    copy(.)
  num_colnames <- colnames(local_d[, .SD, .SDcols = is.numeric])
  local_d[, (num_colnames) := 
            lapply(.SD, function(x) {
              x <- signif(x, digits=3)
              return(x)
            }), .SDcols = num_colnames]
  return(local_d)
}

#' general 
#'
#' @description A utils function
#'
#' @return The return value, if any, from executing the utility.
#'
#' @noRd

get_groups <- function(d, colx) {
  
  if (colx[1] %in% colnames(d)) {
    group_names <- d[, lapply(.SD, unique), 
                     .SDcols = colx]
    
  } else {
    group_names <- ""
  }
  return(group_names)
}

#' general 
#'
#' @description A utils function
#'
#' @return The return value, if any, from executing the utility.
#'
#' @noRd

download_plot <- function(figure, label, input) {
  
  plot_name <- paste0(label, "_", Sys.Date() ,".pdf")
  downloadHandler(
    filename = plot_name,
    content = function(file) {
      ggsave(plot = figure, 
             filename = file, 
             width = input$dl_width, 
             height = input$dl_height, 
             dpi = 300, 
             units = "cm")
    }
  )
}
#' general 
#'
#' @description A utils function
#'
#' @return The return value, if any, from executing the utility.
#'
#' @noRd
str_to_formula <- function(variable, 
                           treatment, 
                           allow_interaction = FALSE, 
                           is_normal = TRUE) {
  
  # Deal with treatment side
  if (length(treatment) > 1) {
    if (allow_interaction) {
      collapse_char <- "*"
    } else {
      collapse_char <- "+"
    }
    treatment_str <- paste(treatment, collapse = collapse_char)
  } else {
    treatment_str <- treatment
  }
  if (is_normal) {
    target_variable <- variable
  } else {
    target_variable <- paste0("ranked_", variable)
  }
  formula_obj <- paste(target_variable, "~", treatment_str) %>%
    formula
}

#' general 
#'
#' @description A utils function
#'
#' @return The return value, if any, from executing the utility.
#'
#' @noRd

formula_to_str <- function(formula) {
  
  # Extract formula elements
  formula_elemens <- formula %>% 
    as.character %>% 
    strsplit(., split = " ~ ") %>% 
    unlist()
  
  # Assign left-hand side
  lhs <- formula_elemens[2]
  
  # Assign right hand side
  rhs <- formula_elemens[3] %>%
    strsplit(., split = " + | * ") %>%
    unlist()
  rhs <- rhs[!rhs %in% c("*", "+")]
  
  return(list(lhs = lhs, rhs = rhs))
}
#' Check N-levels 
#'
#' @description A utils function
#'
#' @return The return value, if any, from executing the utility.
#'
#' @noRd

check_n_levels <- function(d, treatment) {
  
  nlevels <- d[, lapply(.SD, unique), 
               .SDcols = treatment] %>% 
    nrow()
  return(nlevels)
}


#' general 
#'
#' @description A utils function
#'
#' @return The return value, if any, from executing the utility.
#'
#' @noRd
# get_plots <- function(output) {
#   
#   is_plot <- function(x) {
#     !is.null(attr(x, "plotObj"))
#   }
#   
#   plots <- list()
#   for (name in names(output)) {
#     #if (is_plot(output[[name]])) {
#       plots[[name]] <- output[[name]]
#     #}
#   }
#   return(plots)
# }

get_plots <- function(output) {
  plots <- list()
  for (name in names(output)) {
    if (name %in% c("impl", "ns")) {
      next
    }
    if (!is.null(output[[name]])) {
      plots[[name]] <- name
    }
  }
  return(plots)
}