library(shiny)
library(data.table)
library(magrittr)
library(DT)
library(ggplot2)
library(import)
library(ragg)
library(systemfonts)

# Define UI

ui <- shinyUI(
  fluidPage(theme = shinythemes::shinytheme("flatly"),
    titlePanel("Free-Prism"),
    sidebarLayout(
      sidebarPanel(
        textInput("is_perm", "is_perm", "No"),
        textInput("treatment", "treatment", "Species"),
        sliderInput(inputId = "cutoff", label = "cutoff", 
                    min = 0, max = 1, value = 0.05, step = 0.001),
        
        radioButtons("dataset", "Choose a dataset:", 
                     choices = list("Iris" = "iris", "Uploaded File" = "uploaded")),
        conditionalPanel(
          condition = "input.dataset == 'uploaded'",
          fileInput("file1", "Upload a dataset:")
        ),
        actionButton("load_data", "Load Data"),
        br(),
        selectInput("test_type", "Test Type", 
                    choices = c("t-test", "ANOVA"))
      ),
      mainPanel(
        tabsetPanel(
          tabPanel("Exploratory Analysis", 
                   DTOutput("out1"),
                   DTOutput("out2"),
                   plotOutput("hist_plot"),
                   plotOutput("qq_plot")
                   
          ),
          tabPanel("One-way testing",
                   fluidRow(
                     column(3,
                     selectInput("variable", "Select a variable:", 
                               choices = colnames(iris))),
                     column(3,
                            selectInput("treatment", selected = 'Species',
                               "Select a treatment variable:", 
                               choices = colnames(iris))),
                     column(3,
                            selectInput("one_test", 
                               "Select a test:", 
                               choices = c("One-Way-ANOVA",
                                           "Kruskall-Wallis")))
                   ),
                   DTOutput("one_way_test"),
                   selectInput("plot_type", "Select a Plot type:", 
                               choices = c("barplot", "boxplot")), 
                   plotOutput("one_way_plot"),
                   DTOutput("one_way_post"),
                   DTOutput("compare_ls"),
                   
          )
        )
      )
    )
  )
)


# Define server
server <- function(input, output) {
  
  d <- data.table(iris)
  dist_tbl <- reactive(dist_detect(d, input$cutoff))
  #react_method <- analysis_method(dist_tbl(), input)
  
  react_hist_plot <-  reactive(ggplot_hist(data.table(iris), input$treatment))
  react_qq_plot <- reactive(ggplot_qq(d,input$treatment))
  react_summ <- reactive(summary_table(d, input$treatment))
  
  react_comp <- reactive(compare_ls(d = d, 
                           dist_table =  analysis_method(dist_detect(d, input$cutoff), input), 
                           treatment = input$treatment,
                           var_method = NULL))
  
  react_one_way <- reactive(one_way_test(data.table(iris), input))
  one_way_plot <- reactive(
                  plot_one_way(d, 
                                 variable = input$variable, 
                                 treatment = input$treatment,
                                 post_hoc = react_one_way()[[2]], 
                                 ref_group = NULL, 
                                 plot_type = input$plot_type, 
                                 col_palette = "jco")
  )
  
  
  
  # Render outputs
  
  output$method_determine_select <-
    renderUI(reactive(select_manual_method()))
  
   output$out1 <- renderDT({
     analysis_method(dist_detect(data.table(iris), input$cutoff), input)
     }
   )
  
  output$out2 <- renderDT(
    react_summ()
  
  )
  output$out3 <- renderDT(
    react_comp()
  )
  output$one_way_test <- renderDT(react_one_way()[[1]])
  output$one_way_post <- renderDT(react_one_way()[[2]])
  
  
  output$hist_plot <- renderPlot(
    react_hist_plot()
  )
  output$qq_plot <- renderPlot({
    react_qq_plot()
  })
  
  output$one_way_plot <- renderPlot({
    one_way_plot()
  })
  
}


# Run the app
shinyApp(ui = ui, server = server)
