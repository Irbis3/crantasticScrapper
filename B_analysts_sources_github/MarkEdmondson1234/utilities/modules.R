## Shiny modules 

#' dynamicSelectOutput
#' 
#' 
dynamicSelectOutput <- function(id, 
                                label = "",
                                choices = NULL,
                                ...){
  
  ns <- shiny::NS(id)
  
  selectInput(ns("dynamic_select"), label = label, choices = choices, ...)
  
}

#' renderDynamicSelect
#' 
#' @param dataframe A reactive dataframe containing a column of choices
#' @param col_choice The column containing the choices 
#' 
#' @return the choices
dynamicSelect <- function(input, output, session,
                          dataframe, col.choice, ...){
  
  testthat::expect_is(dataframe, "data.frame")
  testthat::expect_is(col.choice, "character")
  
  choice <- reactive({
    d <- dataframe
    choice <- unique(d[col.choice])
    
    choice
  })
  
  observe({
    
    ns <- session$ns
    choice <- choice()
    
    message("Choices: ", paste(choice, collapse = ", "))

    updateSelectInput(external_session, 
                      inputId = ns("dynamic_select"),
                      choices = choice,
                      ...)
  })
  
  return(choice)

}