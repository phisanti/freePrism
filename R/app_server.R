#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function(input, output, session) {
  # Your application server logic
  env_vars <- mod_sidebar_server("sidebar_1")
  d <- mod_data_reader_server("data_reader_1", env_vars)
  mod_explore_server("explore_1", d)
  #mod_explore_server("mod_explore_server", d)
  #callModule(mod_explore_server, "mod_explore_server")
#  mod_explore_server()
}
