library(shiny)
library(leaflet)
# Define UI for application that draws a histogram
shinyUI(fluidPage(

  # Application title
  titlePanel(HTML("GEOSTAT 2015 visualisation competition <a href='https://github.com/Robinlovelace/GEOSTAT2015-viz-challenge'>(see code)</a>")),

  # Sidebar with a slider input for the number of bins
  sidebarLayout(
    sidebarPanel(

      selectInput("dtype", "Display", c("Points", "Raster cells", "Hexbins")),
      # selectInput("mtype", "Trend Model", c("Loess"="lo", "Linear"="li")),
    sliderInput("end_date", dragRange = TRUE,
      "End date:",
      min = strptime("2008-01-01 UTC", format = "%Y-%m-%d", tz = "UTC"),
      max = strptime("2010-12-31 UTC", format = "%Y-%m-%d", tz = "UTC"),
      value = c(strptime("2008-01-01 UTC", format = "%Y-%m-%d", tz = "UTC"),
        strptime("2010-12-31 UTC", format = "%Y-%m-%d", tz = "UTC")),
      timeFormat = "%F"),

      sliderInput("alt", dragRange = TRUE,
        "Altitude (m)",
        min = 0,
        max = 2000,
        value = c(50, 500)),

    sliderInput("span",
                "Rolling mean (months)",
                step = 1,
                min = 1,
                max = 12,
                value = 6),

      selectInput("ptype", "Precipitation type:", choices = c("liquid","solid")),

      textOutput("text")
      ),

    # Show a plot of the generated distribution
    mainPanel(
     leafletOutput(outputId = "map"),
      plotOutput("distPlot")
    )
  )
))