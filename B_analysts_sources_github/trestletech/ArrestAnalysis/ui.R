library(shiny)

# Define UI for application that draws a histogram
shinyUI(fluidPage(
  
  # Application title
  titlePanel("Hello Shiny!"),
  
  # Sidebar with a slider input for the number of bins
  sidebarLayout(
    sidebarPanel(
      sliderInput("windowSize",
                  "Window Size (days):",
                  min = 1,
                  max = 7,
                  value = 2)
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      plotOutput("plot"),
      verbatimTextOutput("model")
    )
  )
))
