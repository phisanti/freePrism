#' Update input selectors for statistical analysis modules
#'
#' This function updates the input selectors for a statistical analysis module
#' based on the data provided. The input selectors include options for selecting
#' the treatment, target variable, reference group, x-variable, y-variable, and
#' color variable for plotting.
#'
#' @param id The module ID.
#' @param d The data to use for updating the input selectors.
#' 
#' @return An observer that updates the input selectors when the data or module ID
#'   changes.
#'
#' @export
updater_module <- function(id, d) {
  moduleServer(id, function(input, output, session) {
    observe({
      
      # Load vars
      modns <- NS(NULL)
      if (id == "linreg") {
        multiple_choice <- ncol(d()) -1
      } else if (id == "twoway") {
        multiple_choice <- 2
      } else {
        multiple_choice <- 1
      }
      all_colnames <- colnames(d())
      all_num <- colnames(d())
      
      # Update main inputs
      updateSelectizeInput(session,
                           modns("treatment"),
                           "Select treatment",
                           options = list(maxItems = multiple_choice),
                           choices = all_colnames)
      updateSelectInput(session,
                        modns("variable"),
                        "Select target variable",
                        selected = all_num[1],
                        choices = all_colnames)
      
      # Special update for reference group
      observeEvent(input$treatment, {
        updateSelectizeInput(session,
                             modns("ref_group"),
                             selected = "",
                             choices = get_groups(d, input$treatment))
      })
      
      # Update plotting inputs
      updateSelectInput(session,
                        modns("xvar"),
                        selected = "",
                        choices = all_colnames)
      updateSelectInput(session,
                        modns("yvar"),
                        selected = all_num[1],
                        choices = all_colnames)
      updateSelectInput(session,
                        modns("colvar"),
                        selected = "",
                        choices = c(" ", all_colnames)
      )
      
    })
  })
}
