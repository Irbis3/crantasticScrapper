library(shiny)

fluidPage(
  titlePanel("A Wildly Creative Exploration of mtcars"),
  sidebarLayout(
    sidebarPanel(
      checkboxGroupInput("cyl", "Cylinders", choices = c(4, 6, 8),
                         selected = c(4, 6, 8))
    ),
    mainPanel(
      plotOutput("plot")
    )
  )
)

