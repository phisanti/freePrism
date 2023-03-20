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
source('R/data_reader.R')
source('R/stat_checks.R')
source('R/compare_means.R')
source('R/compare_means_ui.R')
source('R/one_way_ANOVA.R')
source('R/one_way_ANOVA_ui.R')
source('R/linear_reg.R')
source('R/linear_reg_ui.R')

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
        # Add conditional panels for each type of analysis
        conditionalPanel(
          condition = "input.tabs == 'Exploratory Analysis'",
          ""
        ),
        conditionalPanel(
          condition = "input.tabs == 'Compare means'",
          Compare_means_ui
        ),
        conditionalPanel(
          condition = "input.tabs == 'One-way ANOVA'",
          one_way_ANOVA_ui
        ),
        conditionalPanel(
          condition = "input.tabs == 'Linear Regression'",
          lin_reg_ui
        ),
        textInput("is_perm", "is_perm", "No"),
        selectInput("treatment", "treatment", 
                    choices = ""),
        selectInput("variable", "Select target variable", 
                    choices = ""),
        actionButton("run_analysis", "Run Analysis")
      ),
      
      # Main panels
      mainPanel(
        tabsetPanel(id = 'tabs',
                    
                    tabPanel("Exploratory Analysis", 
                             id = "explore",
                             DTOutput("table"),
                             DTOutput("summ"),
                             DTOutput('dist_tbl'),
                             plotOutput("hist_plot"),
                             plotOutput("qq_plot")
                             
                    ),
                    tabPanel("Compare means", 
                             DTOutput("comp_means_table"),
                             
                             selectInput("plot_type", "Select a Plot type:", 
                                         choices = c("barplot", "boxplot")), 
                             plotOutput("com_m_plot")
                    ),
                    
                    tabPanel("One-way ANOVA",
                             DTOutput("one_way_test"),
                             selectInput("plot_type_ow", "Select a Plot type:", 
                                         choices = c("barplot", "boxplot")), 
                             plotOutput("one_way_plot"),
                             DTOutput("one_way_post"),
                             downloadButton("dl_gg", label = "Download Figure"),
                    ),
                    tabPanel("Linear Regression",
                             fluidRow(column(6, htmlOutput("lm_summary")),
                                      column(6,plotOutput("lm_coefs"))),
                             "Linear Regression",
                             fluidRow(
                               column(3,
                                      selectInput("xvar", "Select the X variable:", 
                                                  choices = "")),
                               column(3,
                                      selectInput("yvar", selected = '',
                                                  "Select the y variable:", 
                                                  choices = "")),
                               column(3,
                                      selectInput("colvar", selected = '',
                                                  "Select a colouring variable:", 
                                                  choices = ""))),
                             plotOutput("lm_pred"),
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
  inputVal <-
    InputValidator$new()
  inputVal$add_rule("df_upload_file", sv_required(message = "Upload a file is required"))
  inputVal$enable()
  d <- reactive(
    read_data(input)
  )
  
  # React exploratory analysis
  react_explore <- eventReactive(input$run_analysis,{
    react_dist_tbl <- dist_detect(d(), 0.05)
    react_summ <- summary_table(d(), input$treatment)
    react_hist_plot <-  ggplot_hist(d(), input$treatment)
    react_qq_plot <- ggplot_qq(d(),input$treatment)
    
    list(react_summ,
         react_dist_tbl,
         react_hist_plot,
         react_qq_plot
    )
  })
  react_two_sample <- eventReactive(input$run_analysis,{
    
    comp_means <- comp_means_test(d(), input)
    com_means_plot <- plot_one_comp_m(d(), input, comp_means)
    
    list(comp_means, com_means_plot)
  })
  # React ANOVA-one-way comparison 
  react_one_way <- eventReactive(input$run_analysis,{
    one_way_test <- one_way_test(d(), input)
    one_way_plot <- 
      plot_one_way(d(),
                   variable = input$variable, 
                   treatment = input$treatment,
                   post_hoc = one_way_test[[2]], 
                   ref.group = NULL, 
                   plot_type = input$plot_type_ow, 
                   col_palette = "jco")
    list(one_way_test[[1]], one_way_test[[2]], one_way_plot)
  })
  # Linear regression
  react_lm <- eventReactive(input$run_analysis,{
    
    lm_test <- reg_test(d(), input)
    lm_plots <- ggplot_lm(d(), lm_test$model, input)
    list(lm_test$model, lm_test$htmlout, lm_plots[[1]], lm_plots[[2]])
  })
  ##### RENDER #####
  # Render content Exploratory Analysis
  output$table <- renderDT(d())
  output$summ <- renderDT( react_explore()[[1]])
  
  output$dist_tbl <- renderDT(react_explore()[[2]])
  output$hist_plot <- renderPlot({
    req(input$treatment %in% colnames(d()))
    react_explore()[[3]]#react_hist_plot()
  })
  output$qq_plot <- renderPlot({
    req(input$treatment %in% colnames(d()))
    react_explore()[[4]]#react_qq_plot()
  })
  
  # Render mean comparison
  output$comp_means_table <- renderDT({
    req(input$treatment != "" & input$treatment != input$variable)
    react_two_sample()[[1]]#react_comp_means()
  })
  output$com_m_plot <- renderPlot({
    req(input$treatment != "" & input$treatment != input$variable)
    react_two_sample()[[2]]
  })
  
  # Render ANOVA-one-way
  output$one_way_test <- renderDT({
    req(input$treatment != "" & input$treatment != input$variable)
    
    react_one_way()[[1]]
  })
  output$one_way_post <- renderDT({
    req(input$treatment_aov != "" & input$treatment != input$variable)
    
    react_one_way()[[2]]
  })
  output$one_way_plot <- renderPlot({
    req(input$treatment != "" & input$treatment != input$variable)
    react_one_way()[[3]]
  })
  
  # Render LM
  output$lm_summary <- renderUI(HTML({
    req(input$treatment != "" & input$treatment != input$variable)
    
    react_lm()[[2]]}))
  output$lm_coefs <- renderPlot(react_lm()[[3]])
  output$lm_pred <- renderPlot(react_lm()[[4]])
  
  observe({
    if (input$tabs == 'Linear Regression') {
      multiple_choice <- ncol(d()) -1
    } else {
      multiple_choice <- 1
    }
    updateSelectizeInput(session, 
                         "treatment", 
                         "Select treatment",
                         options = list(maxItems = multiple_choice),
                         choices = colnames(d()))
    
    updateSelectInput(session, 
                      "variable", 
                      "Select target variable", 
                      selected = colnames(d()[, .SD, .SDcols = is.numeric]), 
                      choices = colnames(d()))
    
    updateSelectInput(session, 
                      "xvar", 
                      selected = "",
                      choices = colnames(d()))
    updateSelectInput(session, 
                      "yvar", 
                      selected = colnames(d()[, .SD, .SDcols = is.numeric]), 
                      choices = colnames(d()))
    updateSelectInput(session, 
                      "colvar", 
                      selected = "",
                      choices = c("", colnames(d()))
    )
    
    updateSelectInput(session,
                      "df_upload_file",
                      selected = input$df_upload_file$datapath)
    
  }
  )
}

# Run the app
shinyApp(ui = ui, server = server)
