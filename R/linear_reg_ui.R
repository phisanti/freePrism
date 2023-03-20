lin_reg_ui <- fluidRow(
  column(3,numericInput(inputId = "cilevel", 
                      label = "Select Conf. Interval: ", 
                      min = 0.01, 
                      max = .99, 
                      value = .95)
         )
  )