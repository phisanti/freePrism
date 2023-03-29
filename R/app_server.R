#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny data.table rstatix ggplot2
#' @importFrom DT renderDT
#' @importFrom ggpubr theme_pubr
#' @importFrom shinyvalidate InputValidator sv_required
#' @noRd

app_server <- function(input, output, session) {
  # Set them
  theme_set(theme_pubr(base_size = 20, base_family = "Arial"))
  
  ## Create reactive objects
  
  # Data reader
  inputVal <-
    InputValidator$new()
  inputVal$add_rule("df_upload_file", sv_required(message = "Upload a file is required"))
  inputVal$enable()
  d <- reactive(
    read_data(input)
  )
  
  explore_module("explore", d)
  comp_means_module("compmeans", d)
  oneway_module("oneway", d)
  twoway_module("twoway", d)
  linreg_module("linreg", d)
  observe(updater_module(input$tabs, d))
  
  output$table <- renderDT(d())

}
