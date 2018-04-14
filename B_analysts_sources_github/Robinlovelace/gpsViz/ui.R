library(shiny)
library(leaflet)

# Define UI for application that draws a histogram
shinyUI(fluidPage(

  # Application title
  titlePanel("Hello Shiny!"),

  # Sidebar with a slider input for the number of bins
  sidebarLayout(
    sidebarPanel(
      shiny::sliderInput("elevation", "Elevation (min, max, m):", min = 0, max = 600, value = c(0, 600)),
      selectInput("modeOfTravel",
                  "Mode of travel:",
                  choices = c("Walk","Cycle","Car")),
      sliderInput("measurementPeriod",
                  "Number of days of app usage",
                  min = 0,
                  max = 700,
                  value = 10
      )
    ),

    # Show a plot of the generated distribution
    mainPanel(
      leafletOutput("map", width = "800", height = "800")
    )
  )
))
