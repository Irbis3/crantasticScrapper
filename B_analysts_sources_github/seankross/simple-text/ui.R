library(shiny)

fluidPage(
  titlePanel("Simple Text Example"),
  sidebarLayout(
    sidebarPanel(
      textInput("phrase1", "Enter a phrase here.", value = "Hello!"),
      actionButton("render", "Render the Text")
    ),
    
    mainPanel(
      textOutput("display")
    )
  )
)

