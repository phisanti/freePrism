# The following function reads the data from the upload file or loads 
# demo data to show the capabilities of the webtool

read_data <- function(input) {
  
  if (input$data_source == "demo-iris") {
    
    d <- data.table(iris)
  } else if (input$data_source == "demo-tooth") {
    
    d <- data.table(ToothGrowth)
  } else if (is.null(input$df_upload_file)) {
    Sys.sleep(20)
  } else {
    
    d <- fread(input$df_upload_file$datapath)
  }
}

