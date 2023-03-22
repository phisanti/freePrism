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
download_figure <- function(figure, label, width, height) {
  
  # Load vars
  width <- input$figwidth
  height <- input$figheight
  
  # Create handler
  downloadHandler(
    filename = paste0(label, "_", Sys.Date() ,".pdf"),
    content = function(file) {
      ggsave(
        file,
        plot = figure,
        width = width,
        height = height, 
        units = "cm"
      )
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