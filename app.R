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
library(remotes)
library(rstatix)
library(ggpubr)
library(golem)
library(glue)
library(DT)
library(freePrism)

# Check package app is installed with all functions
print(lsf.str("package:freePrism"))

# Run app
shiny::shinyApp(
  ui = app_ui,
  server = freePrism::app_server
)
