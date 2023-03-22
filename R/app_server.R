#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny data.table rstatix ggplot2
#' @noRd

app_server <- function(input, output, session) {
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
  # React Two-way ANOVA
  react_two_way <- eventReactive(input$run_analysis, {
    
    two_ANOVA <- two_ANOVA(d(), 
                           input$treatment, 
                           input$variable, 
                           input$allow_interaction,
                           input$is_normal,
                           ANOVA_type = input$anova_type) 
    posthoc <- post_hoc_two_way(d(),
                      input$treatment,
                     input$variable,
                     input$allow_interaction,
                     input$is_normal,
                     input$p_adjust_method)
    print(input$plot_analysis)
    print(input$xval)
    print(input$yval)
    print(input$colval)
    print(input$plot_type)
    
    list(two_ANOVA, posthoc)
  })
  
  #reac_twowayplot <- 
    reactive({
      
    #print(react_two_way()[[2]])
    print(input$plot_analysis)
      print(input$xval)
    print(input$yval)
    print(input$colval)
    print(input$plot_type)
      
    # twaplot <- plot_two_way(d(), 
    #                      input$xval,
    #                      input$yval, 
    #                      input$colval,
    #                      post_hoc = react_two_way()[[2]], 
    #                      ref.group = NULL, 
    #                      plot_type = input$plot_type, 
    #                      col_palette = "jco")
    # print(twaplot)
    #list(twaplot)
  })
  
  
  # React Linear regression
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
  output$owplot_dl <- eventReactive(input$run_analysis, {
    req(!is.null(output$mult_comp))
    download_figure(react_two_sample()[[2]], "mult_comp", input)
  })
  # Render ANOVA-one-way
  output$one_way_test <- renderDT({
    req(input$treatment != "" & input$treatment != input$variable)
    
    react_one_way()[[1]]
  })
  output$one_way_post <- renderDT({
    req(input$treatment != "" & input$treatment != input$variable)
    
    react_one_way()[[2]]
  })
  output$one_way_plot <- renderPlot({
    req(input$treatment != "" & input$treatment != input$variable)
    react_one_way()[[3]]
  })
  output$owplot_dl <- eventReactive(input$run_analysis, {
    req(!is.null(output$mult_comp))
    
    download_figure(react_one_way()[[3]], "mult_comp", input)
    })
  
  # Render Two-way ANOVA
  output$two_way_DT <- renderDT({
    req(input$treatment != "" & input$treatment != input$variable)
    react_two_way()[[1]]
  })
  output$twowaypost_DT <- renderDT({
    req(input$treatment != "" & input$treatment != input$variable)
    react_two_way()[[2]]
  })
  output$twowayplot <- renderPlot({
    req(input$xval != "" & input$yval != "")
    
    reac_twowayplot()[[1]]
  })
  
  
  # Render LM
  output$lm_summary <- renderUI(HTML({
    req(input$treatment != "" & input$treatment != input$variable)
    
    react_lm()[[2]]}))
  output$lm_coefs_plot <- renderPlot(react_lm()[[3]])
  output$lm_pred_plot <- renderPlot(react_lm()[[4]])
  
  # Update input items
  observeEvent(input$treatment, {      
    updateSelectizeInput(session,
                         "ref_group",
                         selected = "",
                         choices = get_groups(d(), input$treatment))
  })
  
  observe({
    if (input$tabs == "Linear Regression") {
      multiple_choice <- ncol(d()) -1
    } else if (input$tabs == "Two-way-ANOVA") {
      multiple_choice <- 2
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
                      choices = c(" ", colnames(d()))
    )
    
    updateSelectInput(session,
                      "df_upload_file",
                      selected = input$df_upload_file$datapath)
    
  }
  )
  
}
