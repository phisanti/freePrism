########################################
# Render
#    panel: main - DataViewer
#    rct_df_data - DT
########################################

#output$df_com <-
#  renderDT(rct_df_data())

########################################
# Render
#    panel: main - Distribution Determine
###    rct_dist_detect - DT
###    rct_analysis_method - DT
#    rct_df_dist_n_method - DT
#    rct_ggplot_hist - plot
#    rct_ggplot_qq - plot
########################################

library(DT)


output$df_dist_n_method <-
  renderDT(reactive(df_dist_n_method(
    # Add args
  )))

output$method_determine_select <-
  renderUI(reactive(select_method_determine(
    # Add args
  )))

output$ggplot_hist <-
  renderPlot(reactive(ggplot_hist(
    # Add args
  )))
output$ggplot_qq <-
  renderPlot(reactive(rct_ggplot_qq(
    # Add args
  )))