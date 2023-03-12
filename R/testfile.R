
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
                   DTOutput("table"),
                   DTOutput("summ"),
                   DTOutput('dist_tbl'),
                   plotOutput("hist_plot"),
                   plotOutput("qq_plot")
                   
          ),
          tabPanel("Compare means", 
                   
                   fluidRow(
                     column(3,
                            selectInput("treatment_means", "Select groups to compare", 
                                        choices = ""),
                            selectInput("variable", "Select target variable", 
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
                   plotOutput("com_m_plot")
          ),
          
          tabPanel("One-way testing",
                   fluidRow(
                     column(3,
                            selectInput("variable_aov", "Select a variable:", 
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
  inputVal <-
    InputValidator$new()
  inputVal$add_rule("df_upload_file", sv_required(message = "Upload a file is required"))
  inputVal$enable()
  d <- reactive(
    read_data(input)
    )
  
  # React exploratory analysis
  react_dist_tbl <- reactive(dist_detect(d(), 0.05))
  react_hist_plot <-  reactive(ggplot_hist(d(), input$treatment))
  react_qq_plot <- reactive(ggplot_qq(d(),input$treatment))
  react_summ <- reactive(summary_table(d(), input$treatment))
  
  # React mean comparison 
  react_comp_means <- reactive(comp_means_test(d(), input))
  react_com_m_plot <- reactive(plot_one_comp_m(d(), input, react_comp_means()))
  
  # React ANOVA-one-way comparison 
  react_one_way <- reactive(one_way_test(d(), input))
  react_one_way_plot <- reactive(
    plot_one_way(d(),
                 variable = input$variable_aov, 
                 treatment = input$treatment_aov,
                 post_hoc = react_one_way()[[2]], 
                 ref.group = NULL, 
                 plot_type = input$plot_type_ow, 
                 col_palette = "jco")
  )
  
    
  ##### RENDER #####
  # Render content Exploratory Analysis
  output$table <- renderDT(d())
  output$summ <- renderDT(react_summ())
  
  output$dist_tbl <- renderDT(react_dist_tbl())
  output$hist_plot <- renderPlot({
    req(input$treatment %in% colnames(d()))
    react_hist_plot()
    })
  output$qq_plot <- renderPlot({
    req(input$treatment %in% colnames(d()))
    react_qq_plot()})
  
  
  
  # Render mean comparison
  output$comp_means_table <- renderDT({
    req(input$treatment_means != "" & input$treatment_means != input$variable)
    react_comp_means()
    })
  output$com_m_plot <- renderPlot({
    req(input$treatment_means != "" & input$treatment_means != input$variable)
    react_com_m_plot()
    })
  
  # Render ANOVA-one-way
  output$one_way_test <- renderDT({
    req(input$treatment_aov != "" & input$treatment_aov != input$variable_aov)
    
    react_one_way()[[1]]
})
  output$one_way_post <- renderDT({
    req(input$treatment_aov != "" & input$treatment_aov != input$variable_aov)
    
    react_one_way()[[2]]
})
  output$one_way_plot <- renderPlot({
    req(input$treatment_aov != "" & input$treatment_aov != input$variable_aov)
    react_one_way_plot()
  })
  
  
  
  observe({
    updateSelectInput(session, 
                      "treatment", 
                      "Select treatment", 
                      choices = colnames(d()))
    
    updateSelectInput(session, 
                      "treatment_means", 
                      "Select groups to compare", 
                      selected = colnames(d()[, .SD, .SDcols = is.character]),
                      choices = colnames(d()))
    updateSelectInput(session, 
                      "variable", 
                      "Select target variable", 
                      selected = colnames(d()[, .SD, .SDcols = is.numeric]), 
                      choices = colnames(d()))
    
    
    updateSelectInput(session, 
                      "treatment_aov", 
                      "Select groups to compare", 
                      selected = colnames(d()[, .SD, .SDcols = is.character]),
                      choices = colnames(d()))
    updateSelectInput(session, 
                      "variable_aov", 
                      "Select target variable", 
                      selected = colnames(d()[, .SD, .SDcols = is.numeric]), 
                      choices = colnames(d()))
    
    updateSelectInput(session,
      "df_upload_file",
      selected = input$df_upload_file$datapath)

  }
  )
}

# Run the app
shinyApp(ui = ui, server = server)
