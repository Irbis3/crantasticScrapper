library(shiny)
library(dplyr)


ui <- fluidPage(
  titlePanel("Orders"),
  sidebarLayout(
    sidebarPanel(
      selectInput("Region_Input", label = h5("Choose a Region"),
                  choices = list("A", "B")),
      radioButtons("level", "What level do you want to see:",
                   list("cyl", "am"))

    ),
    mainPanel(
      verbatimTextOutput("Level_Select"),
      tableOutput(outputId="table")

    )))


server <-  function(input, output) {
    output$table <- renderTable({
      mtcars %>%
        group_by_(input$level) %>%
        summarize(count = n()) %>%
        arrange(desc(count))
    })
  }

shinyApp(ui = ui, server = server)
