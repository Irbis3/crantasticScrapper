library(dygraphs)

shinyUI(fluidPage(
  
  titlePanel("Lung disease data"),
  
  sidebarLayout(
    sidebarPanel(
      numericInput("months", label = "Months to Predict", 
                   value = 48, min = 12, max = 144, step = 12),
      selectInput("interval", label = "Prediction Interval",
                  choices = c("0.75, 0.80", "0.90", "0.95", "0.99"),
                  selected = "0.95"),
      checkboxInput("showgrid", label = "Show Grid", value = TRUE)
    ),
    mainPanel(
      dygraphOutput("dygraph")
    )
  )
))