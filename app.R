#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(remotes)
library(shinyvalidate)
library(shinythemes)
library(shiny)
library(data.table)
library(stargazer)
library(ggfortify)
library(rstatix)
library(ggpubr)
library(golem)
library(glue)
library(DT)

# install freePrism if required
if (magrittr::not("freePrism" %in% rownames(installed.packages()))) {
  remotes::install_github('phisanti/freePrism')
}
library(freePrism)

# Run aoo
shiny::shinyApp(
  ui = freePrism::app_ui,
  server = freePrism::app_server
)
