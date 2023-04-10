#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
library(remotes)
remotes::install_github('phisanti/freePrism')
library(freePrism)
shiny::shinyApp(
  ui = app_ui,
  server = app_server
)
