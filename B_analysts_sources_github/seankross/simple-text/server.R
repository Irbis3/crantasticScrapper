library(shiny)

function(input, output) {
  makeText <- eventReactive(input$render, {
    paste("The text box says:", input$phrase1)
  })

  output$display <- renderText(makeText())
}
