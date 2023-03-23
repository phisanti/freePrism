#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#'     DO NOT REMOVE.
#' @import shiny
#' @importFrom shiny fluidPage sidebarLayout sidebarPanel radioButtons helpText 
#' conditionalPanel textInput selectInput actionButton mainPanel
#' @noRd

library(shiny)

app_ui <- function(request) {
  golem_add_external_resources()
  shiny::shinyUI(
  
  #### Create User Interface
  shiny::fluidPage(
    # Load theme and heading
    theme = shinythemes::shinytheme("flatly"),
    shiny::titlePanel("Free-Prism"),
    # Side panel
    shiny::sidebarLayout(
      shiny::sidebarPanel(
        shiny::radioButtons(
          "data_source", "Upload files or try the demo",
          choices = c(
            "Upload files" = "file",
            "Iris Data (Demo1)" = "demo-iris",
            "ToothGrowth Data (Demo2)" = "demo-tooth"
          ),
          selected = "demo-tooth"
        ),
        shiny::helpText(
          shiny::a(href = "https://archive.ics.uci.edu/ml/datasets/iris", "Iris Data Set"),
          shiny::tags$br(),
          shiny::a(href = "https://academic.oup.com/jn/article-abstract/33/5/491/4726758?redirectedFrom=fulltext", "ToothGrowth Data Set")
        ),
        h1("Statistical analysis tools"),
        shiny::conditionalPanel(
          condition = "input.data_source == 'file'",
          shiny::fileInput(
            inputId = "df_upload_file",
            label = NULL,
            accept = ".csv"
          )
        ),
        # Add conditional panels for each type of analysis
        shiny::conditionalPanel(
          condition = "input.tabs == 'Exploratory Analysis'",
          ""
        ),
        shiny::conditionalPanel(
          condition = "input.tabs == 'Compare means'",
          sidebar_elements_ui[["compmean"]]
        ),
        shiny::conditionalPanel(
          condition = "input.tabs == 'One-way ANOVA'",
          sidebar_elements_ui[["oneway"]]
        ),
        shiny::conditionalPanel(
          condition = "input.tabs == 'Two-way-ANOVA'",
          sidebar_elements_ui[["twoway"]]
        ),
        shiny::conditionalPanel(
          condition = "input.tabs == 'Linear Regression'",
          sidebar_elements_ui[["linreg"]]
        ),
        shiny::selectInput("treatment", "treatment", 
                    choices = ""),
        shiny::selectInput("variable", "Select target variable", 
                    choices = ""),
        shiny::fluidRow(
          shiny::column(5,
        shiny::actionButton("run_analysis", "Run Analysis")),
        shiny::column(5,
                      shiny::actionButton("plot_analysis", "Plot Analysis"))
        ),
        h1("Plotting tools"),
        shiny::conditionalPanel(
          condition = "input.tabs == 'Exploratory Analysis'",
          sidebar_elements_ui$plot_tools[["exploratory"]]
        ),
        shiny::conditionalPanel(
          condition = "input.tabs == 'Compare means'",
          sidebar_elements_ui$plot_tools[["meancomp"]]
        ),
        shiny::conditionalPanel(
          condition = "input.tabs == 'One-way ANOVA'",
          sidebar_elements_ui$plot_tools[["oneway"]]
        ),
        shiny::conditionalPanel(
          condition = "input.tabs == 'Two-way-ANOVA'",
          sidebar_elements_ui$plot_tools[["twoway"]]
        ),
        shiny::conditionalPanel(
          condition = "input.tabs == 'Linear Regression'",
          sidebar_elements_ui$plot_tools[["linreg"]]
        ),
        
      ),
      
      # Main panels
      shiny::mainPanel(
        shiny::tabsetPanel(id = 'tabs',
                   # select_test_ui,
                    
                    shiny::tabPanel("Exploratory Analysis", 
                             id = "explore",
                             DTOutput("table"),
                             DTOutput("summ"),
                             DTOutput('dist_tbl'),
                             plotOutput("hist_plot"),
                             plotOutput("qq_plot")
                             
                    ),
                    shiny::tabPanel("Compare means", 
                                    h1("Mean comparison test"),
                                    p(mainpanel_txt_ui$compare_means$intro[1]),
                                    p(mainpanel_txt_ui$compare_means$intro[2]),
                                    p(mainpanel_txt_ui$compare_means$intro[3]),
                             DTOutput("comp_means_table"),
                             plotOutput("com_m_plot"),
                             downloadButton("com_m_plot_dl", 
                                            label = "Download Figure"),
                             
                    ),
                    
                    shiny::tabPanel("One-way ANOVA",
                                    h1("Multiple comparison test"),
                                    p(mainpanel_txt_ui$one_anova$intro[1]),
                                    p(mainpanel_txt_ui$one_anova$intro[2]),
                                    p(mainpanel_txt_ui$one_anova$intro[3]),
                                    p(mainpanel_txt_ui$one_anova$intro[4]),
                             DTOutput("one_way_test"),
                             plotOutput("one_way_plot"),
                             downloadButton("one_way_plot_dl", 
                                            label = "Download Figure"),
                             DTOutput("one_way_post"),
                    ),
                    shiny::tabPanel("Two-way-ANOVA",
                                    h1("Two-wat ANOVA"),
                                    p(mainpanel_txt_ui$two_anova$intro[1]),
                                    p(mainpanel_txt_ui$two_anova$intro[2]),
                                    p(mainpanel_txt_ui$two_anova$intro[3]),
                                    p(mainpanel_txt_ui$two_anova$intro[4]),
                                    
                             DTOutput("two_way_DT"),
                             DTOutput("twowaypost_DT"),
                             plotOutput("twowayplot"),
                             downloadButton("twowayplot_dl", 
                                            label = "Download Figure"),
                             
                             
                    ),
                    shiny::tabPanel("Linear Regression",
                                    h1("Linear regression"),
                                    p(mainpanel_txt_ui$lin_reg$intro[1]),
                                    p(mainpanel_txt_ui$lin_reg$intro[2]),
                                    p(mainpanel_txt_ui$lin_reg$intro[3]),
                                    p(mainpanel_txt_ui$lin_reg$intro[4]),
                                    
                                    shiny::fluidRow(column(6, htmlOutput("lm_summary")),
                                      column(6,plotOutput("lm_coefs_plot"))),
                             "Linear Regression",
                             )
                    )
        )
      )
    )
  )
}

#' Add external Resources to the Application
#'
#' This function is internally used to add external
#' resources inside the Shiny application.
#'
#' @import shiny
#' @importFrom golem add_resource_path activate_js favicon bundle_resources
#' @noRd
golem_add_external_resources <- function() {
  add_resource_path(
    "www",
    app_sys("app/www")
  )
  
  tags$head(
    favicon(),
    bundle_resources(
      path = app_sys("app/www"),
      app_title = "freePrism"
    )
    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert()
  )
}
