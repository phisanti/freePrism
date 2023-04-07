#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#'     DO NOT REMOVE.
#' @import shiny
#' @importFrom shiny fluidPage sidebarLayout sidebarPanel radioButtons helpText 
#' conditionalPanel textInput selectInput actionButton mainPanel
#' @importFrom DT DTOutput
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
          condition = "input.tabs == 'explore'",
          explore_ui("explore", "stats")
        ),
        shiny::conditionalPanel(
          condition = "input.tabs == 'compmeans'",
          compmeans_ui("compmeans", "stats")
        ),
        shiny::conditionalPanel(
          condition = "input.tabs == 'oneway'",
          oneway_ui("oneway", "stats")
        ),
        shiny::conditionalPanel(
          condition = "input.tabs == 'twoway'",
          twoway_ui("twoway", "stats")
        ),
        shiny::conditionalPanel(
          condition = "input.tabs == 'linreg'",
          linreg_ui("linreg", "stats")
        ),
        h1("Plotting tools"),
        shiny::conditionalPanel(
          condition = "input.tabs == 'explore'",
          explore_ui("explore", "plotting")
        ),
        shiny::conditionalPanel(
          condition = "input.tabs == 'compmeans'",
          compmeans_ui("compmeans", "plotting")
        ),
        shiny::conditionalPanel(
          condition = "input.tabs == 'oneway'",
          oneway_ui("oneway", "plotting")
        ),
        shiny::conditionalPanel(
          condition = "input.tabs == 'twoway'",
          twoway_ui("twoway", "plotting")
        ),
        shiny::conditionalPanel(
          condition = "input.tabs == 'linreg'",
          linreg_ui("linreg", "plotting")
        ),
      ),
      
      # Main panels
      shiny::mainPanel(
        shiny::tabsetPanel(id = 'tabs', 
                   # select_test_ui,
                    
                    shiny::tabPanel("Exploratory Analysis", 
                                    value = "explore",
                             DTOutput("table"),
                             DTOutput("explore-summ"),
                             DTOutput('explore-dist_tbl'),
                             plotOutput("explore-plot"),#,
                             downloadButton("explore-plot_dl", 
                                            label = "Download Figure"),                             
                    ),
                    shiny::tabPanel(title = "Compare means", 
                                    value = "compmeans",
                                    h1("Mean comparison test"),
                                    p(mainpanel_txt_ui$compare_means$intro[1]),
                                    p(mainpanel_txt_ui$compare_means$intro[2]),
                                    p(mainpanel_txt_ui$compare_means$intro[3]),
                             uiOutput("compmeans-text"),
                             DTOutput("compmeans-comp_means_table"),
                             plotOutput("compmeans-com_m_plot"),
                             downloadButton("compmeans-plot_dl", 
                                            label = "Download Figure"),
                             
                    ),
                    
                    shiny::tabPanel("One-way ANOVA",
                                    value = "oneway",
                                    h1("Multiple comparison test"),
                                    p(mainpanel_txt_ui$one_anova$intro[1]),
                                    p(mainpanel_txt_ui$one_anova$intro[2]),
                                    p(mainpanel_txt_ui$one_anova$intro[3]),
                                    p(mainpanel_txt_ui$one_anova$intro[4]),
                                    uiOutput("oneway-test_text"),
                                    DTOutput("oneway-one_way_test"),
                                    uiOutput("oneway-posthoc_text"),
                                    DTOutput("oneway-one_way_post"),
                                    plotOutput("oneway-one_way_plot"),
                             downloadButton("oneway-plot_dl", 
                                            label = "Download Figure")
                    ),
                    shiny::tabPanel("Two-way-ANOVA",
                                    value = "twoway",
                                    h1("Two-wat ANOVA"),
                                    p(mainpanel_txt_ui$two_anova$intro[1]),
                                    p(mainpanel_txt_ui$two_anova$intro[2]),
                                    p(mainpanel_txt_ui$two_anova$intro[3]),
                                    p(mainpanel_txt_ui$two_anova$intro[4]),
                                    uiOutput("twoway-test_text"),
                                    
                             DTOutput("twoway-test"),
                             uiOutput("twoway-posthoc_text"),
                             DTOutput("twoway-posthoc"),
                             plotOutput("twoway-plot"),
                             downloadButton("twoway-plot_dl", 
                                            label = "Download Figure"),
                             
                             
                    ),
                    shiny::tabPanel("Linear Regression",
                                    value = "linreg",
                                    h1("Linear regression"),
                                    p(mainpanel_txt_ui$lin_reg$intro[1]),
                                    p(mainpanel_txt_ui$lin_reg$intro[2]),
                                    p(mainpanel_txt_ui$lin_reg$intro[3]),
                                    uiOutput("linreg-text"),
                                    shiny::fluidRow(column(6, 
                                                           htmlOutput("linreg-model")),
                                      column(6,plotOutput("linreg-coefs_plot"))),
                             plotOutput("linreg-pred_plot"),
                             downloadButton("linreg-plot_dl", 
                                            label = "Download Figure"),
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
