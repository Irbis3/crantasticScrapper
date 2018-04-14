library(shiny)
library(dplyr)

function(input, output){
  mtsupercars <- reactive({
    mtcars %>%
      filter(cyl %in% input$cyl)
  })
  
  output$plot <- renderPlot({
    plot(mtsupercars()$mpg, mtsupercars()$hp, xlim = c(10, 40), ylim = c(50, 350))
  })
}