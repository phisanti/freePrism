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
          sidebar_elements_ui[["comp_means_ui"]]
        ),
        shiny::conditionalPanel(
          condition = "input.tabs == 'One-way ANOVA'",
          sidebar_elements_ui[["one_way_ANOVA_ui"]]
        ),
        shiny::conditionalPanel(
          condition = "input.tabs == 'Two-way-ANOVA'",
          sidebar_elements_ui[["two_way_ANOVA_ui"]]
        ),
        shiny::conditionalPanel(
          condition = "input.tabs == 'Linear Regression'",
          sidebar_elements_ui[["lin_reg_ui"]]
        ),
        shiny::textInput("is_perm", "is_perm", "No"),
        shiny::selectInput("treatment", "treatment", 
                    choices = ""),
        shiny::selectInput("variable", "Select target variable", 
                    choices = ""),
        shiny::actionButton("run_analysis", "Run Analysis")
      ),
      
      # Main panels
      shiny::mainPanel(
        shiny::tabsetPanel(id = 'tabs',
                    select_test_ui,
                    
                    shiny::tabPanel("Exploratory Analysis", 
                             id = "explore",
                             DTOutput("table"),
                             DTOutput("summ"),
                             DTOutput('dist_tbl'),
                             plotOutput("hist_plot"),
                             plotOutput("qq_plot")
                             
                    ),
                    shiny::tabPanel("Compare means", 
                             DTOutput("comp_means_table"),
                             
                             selectInput("plot_type", "Select a Plot type:", 
                                         choices = c("barplot", "boxplot")), 
                             plotOutput("com_m_plot")
                    ),
                    
                    shiny::tabPanel("One-way ANOVA",
                             DTOutput("one_way_test"),
                             selectInput("plot_type_ow", "Select a Plot type:", 
                                         choices = c("barplot", "boxplot")), 
                             plotOutput("one_way_plot"),
                             DTOutput("one_way_post"),
                             downloadButton("dl_gg", label = "Download Figure"),
                    ),
                    shiny::tabPanel("Two-way-ANOVA",
                             tableOutput("two_way_DT")
                    ),
                    shiny::tabPanel("Linear Regression",
                                    shiny::fluidRow(column(6, htmlOutput("lm_summary")),
                                      column(6,plotOutput("lm_coefs"))),
                             "Linear Regression",
                             shiny::fluidRow(
                               shiny::column(3,
                                      selectInput("xvar", "Select the X variable:", 
                                                  choices = "")),
                               shiny::column(3,
                                      selectInput("yvar", selected = '',
                                                  "Select the y variable:", 
                                                  choices = "")),
                               shiny::column(3,
                                      selectInput("colvar", selected = '',
                                                  "Select a colouring variable:", 
                                                  choices = ""))),
                             shiny::plotOutput("lm_pred"),
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
