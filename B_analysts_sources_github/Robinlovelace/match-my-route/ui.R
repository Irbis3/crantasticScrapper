
# This is the user-interface definition of a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)
library(leaflet)

shinyUI(fluidPage(

  # Application title
  titlePanel("Match MY Route"),
  tabsetPanel(
  tabPanel("Your Routes",

  # Sidebar with a slider input for number of bins
  sidebarLayout(
    sidebarPanel(
      textInput("orig", "Travel from", "M3 4EE"),
      textInput("dest", "Travel to", "M1 4BT")
    ),

    # Show a plot of the generated distribution
    mainPanel(
      leafletOutput("map")
    )
  )),
  tabPanel("Matching routes",

    sidebarLayout(
      sidebarPanel(
        textInput("ppl", "Route type", "Easy"),
        textInput("ppl", "Guide", "Cyclist")
      ),

      # Show a plot of the generated distribution
      mainPanel(
        leafletOutput("map2")
      )
    )

    )
)))
