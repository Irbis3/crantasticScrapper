library(shiny)
library(googlesheets)
library(magrittr)

function(input, output){
  if(is.null(gs_ls("guess_the_number"))){
    guess_sheet <- gs_new(title = "guess_the_number", row_extent = 2, col_extent = 2) %>%
      gs_edit_cells(input = data.frame(Guess = 0, Time = 0))
  } else {
    guess_sheet <- gs_title("guess_the_number")
  }
  
  observeEvent(input$action, {
    toggle("response")
    toggle("guess")
    toggle("action")
    gs_add_row(guess_sheet, ws = "Sheet1", input = c(input$guess, as.character(Sys.time())))
  })
  
  output$response <- renderText(paste0("You guessed ", input$guess, 
                                       ". Your response has been recorded."))
}