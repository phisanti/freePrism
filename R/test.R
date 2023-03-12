library(shiny)
library(data.table)
library(magrittr)
library(DT)
library(ggplot2)
library(import)
library(ragg)
library(systemfonts)
library(ggpubr)
library(rstatix)
library(shinyvalidate)
source('R/one_way_ANOVA.R')
source('R/compare_means.R')
source('R/data_reader.R')
source('R/stat_checks.R')

# Define UI

ui <- shinyUI(
  #### Create User Interface
  fluidPage(
  # Load theme and heading
    theme = shinythemes::shinytheme("flatly"),
    titlePanel("Free-Prism"),
    # Side panel
    sidebarLayout(
      sidebarPanel(
        
        radioButtons(
          "data_source", "Upload files or try the demo",
          choices = c(
            "Upload files" = "file",
            "Iris Data (Demo1)" = "demo-iris",
            "ToothGrowth Data (Demo2)" = "demo-tooth"
          ),
          selected = "demo-tooth"
        ),
        helpText(
          a(href = "https://archive.ics.uci.edu/ml/datasets/iris", "Iris Data Set"),
          tags$br(),
          a(href = "https://academic.oup.com/jn/article-abstract/33/5/491/4726758?redirectedFrom=fulltext", "ToothGrowth Data Set")
        ),
        conditionalPanel(
          condition = "input.data_source == 'file'",
          fileInput(
            inputId = "df_upload_file",
            label = NULL,
            accept = ".csv"
          )
        ),
        textInput("is_perm", "is_perm", "No"),
        selectInput("treatment", "treatment", 
                  choices = ""),
      ),
      
      # Main panels
      mainPanel(
        tabsetPanel(
          tabPanel("Exploratory Analysis", 
                   DTOutput("out1"),
                   DTOutput("out2"),
                   DTOutput("out3"),
                   plotOutput("hist_plot"),
                   plotOutput("qq_plot")
                   
          ),
          

          tabPanel("Compare means", 
                   fluidRow(
                     column(3,
                            selectInput("treatment", "treatment_means", 
                                        choices = ""),
                            selectInput("ref_group", "Select a control group:", 
                                        choices = NULL)),
                     column(3,
                            selectInput("var_equal", selected = TRUE,
                                        "Are variances equal:", 
                                        choices = c(TRUE, FALSE))),
                     column(3,
                            selectInput("paired", selected = FALSE,
                                        "Is the test paired:", 
                                        choices = c(TRUE, FALSE))),
                     column(3,
                            selectInput("mean_test", 
                                        "Select a test:", 
                                        choices = c("T-test",
                                                    "Wilcox-test"))),
                     column(3,
                            selectInput("alt_h", 
                                        "Select a test:", 
                                        choices = c("two.sided", "less", "greater")))
                     
                   ),
                   DTOutput("comp_means_table"),
                   selectInput("plot_type", "Select a Plot type:", 
                               choices = c("barplot", "boxplot")), 
                   
                   plotOutput("com_m_plot"),
                   downloadButton("dl_gg", label = "Download Figure"),
                   
          ),
          tabPanel("One-way testing",
                   fluidRow(
                     column(3,
                     selectInput("variable", "Select a variable:", 
                               choices = "")),
                     column(3,
                            selectInput("treatment_aov", selected = 'Species',
                               "Select a target variable:", 
                               choices = "")),
                     column(3,
                            selectInput("one_test", 
                               "Select a test:", 
                               choices = c("One-Way-ANOVA",
                                           "Kruskall-Wallis")))
                   ),
                   DTOutput("one_way_test"),
                   selectInput("plot_type_ow", "Select a Plot type:", 
                               choices = c("barplot", "boxplot")), 
                   plotOutput("one_way_plot"),
                   DTOutput("one_way_post"),
                   DTOutput("compare_ls"),
                   downloadButton("dl_gg", label = "Download Figure"),
          )
        )
      )
    )
  )
)


# Define server
server <- function(input, output, session) {
  
  # Set them
  theme_set(theme_pubr(base_size = 20, base_family = "Arial"))
  
  ## Create reactive objects
  
  # Data reader
  d <- reactive(read_data(input))
  observe({
    req(d())
    updateSelectInput(session, 
                      "treatment", 
                      "Select treatment", 
                      choices = colnames(d()))
    updateSelectInput(session, 
                      "treatment_means", 
                      "Select treatment", 
                      choices = colnames(d()))
    
    updateSelectInput(session, 
                      "treatment_aov", 
                      "Select treatment", 
                      choices = colnames(d()))
    updateSelectInput(session, 
                      "variable", 
                      "Select variable", 
                      choices = colnames(d()))
    }
    )
  dist_tbl <- reactive(dist_detect(d(), input$cutoff))
  react_hist_plot <-  reactive(ggplot_hist(d(), input$treatment))
  react_qq_plot <- reactive(ggplot_qq(d(),input$treatment))
  react_summ <- reactive(summary_table(d(), input$treatment))
  
  
  
  react_comp <- reactive(compare_ls(d = d(), 
                           dist_table =  analysis_method(dist_detect(d(), input$cutoff), input), 
                           treatment = input$treatment,
                           var_method = NULL))
  
  react_comp_means <- reactive(comp_means_test(d(), input))
  react_com_m_plot <- reactive({
        plot_one_comp_m(d(), input, react_comp_means())
  }
  )
  
#  observe({
#    )
  react_one_way <- reactive(one_way_test(d(), input))
  one_way_plot <- reactive(
#                req(input$treatment)
                  plot_one_way(d(), 
                                 variable = input$variable, 
                                 treatment = input$treatment_aov,
                                 post_hoc = react_one_way()[[2]], 
                                 ref.group = NULL, 
                                 plot_type = input$plot_type_ow, 
                                 col_palette = "jco")
  )
  
  
  
  # Render outputs
  
  output$method_determine_select <-
    renderUI(reactive(select_manual_method()))
  
   output$out1 <- renderDT({
     dist_tbl()
     #analysis_method(dist_detect(d(), input$cutoff), input)
     }
   )
  
  output$out2 <- renderDT(
    react_summ()
  
  )
  output$out3 <- renderDT(
    react_comp()
  )
  
  output$comp_means_table <- renderDT(react_comp_means())
  output$one_way_test <- renderDT(react_one_way()[[1]])
  output$one_way_post <- renderDT(react_one_way()[[2]])
  
  
  output$hist_plot <- renderPlot(
    react_hist_plot()
  )
  output$qq_plot <- renderPlot({
    react_qq_plot()
  })
  
  output$com_m_plot <- renderPlot({
    react_com_m_plot()
  })
  
  output$one_way_plot <- renderPlot({
    one_way_plot()
  })
  
  output$dl_gg <-
    downloadHandler(
      filename = "post_hoc_figure.pdf",
      content = function(file) {
        ggsave(
          file,
          plot = one_way_plot()
          )
      }
    )
}

# Run the app
shinyApp(ui = ui, server = server)


ui <- fluidPage(
  
  sidebarLayout(
    sidebarPanel(
      selectInput("data_type", "Data type:",
                  choices = c("Continuous", "Discrete")),
      conditionalPanel(
        condition = "input.data_type == 'Continuous'",
        selectInput("num_ind_vars", "Number of independent variables:",
                    choices = 1:2),
        conditionalPanel(
          condition = "input.num_ind_vars == '1'",
          numericInput("num_groups", "Number of groups:", min = 2, max = 10, value = 2)
        ),
        conditionalPanel(
          condition = "input.num_ind_vars == '2'",
          h4("Output for two independent variables goes here")
        )
      ),
      conditionalPanel(
        condition = "input.data_type == 'Discrete'",
        h4("Output for discrete data goes here")
      )
    ),
    mainPanel(
      h4("Statistical test output goes here")
    )
  )
)

server <- function(input, output) {}
# Run the app
shinyApp(ui = ui, server = server)

