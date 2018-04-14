library(shiny)
library(leaflet)
# Define UI for application that draws a histogram
shinyUI(fluidPage(

  # Application title
  titlePanel(HTML("Intensive Poultry Units in Hereford <a href='http://www.r5r.eu/ipu/'>(see here for more)</a>")),

  # Sidebar with a slider input for the number of bins
  sidebarLayout(
    sidebarPanel(
      selectInput(inputId = "yr", label = "Year erected", choices = c("After 2000" = "y", "Before 2000" = "n", "All units" = "y|n"),  selected = "After 2000")
    ),

    # Show a plot of the generated distribution
    mainPanel(
      leafletOutput(outputId = "map")
    )
  )
))