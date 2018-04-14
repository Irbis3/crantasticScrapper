library("shiny")

shinyUI(pageWithSidebar(
  titlePanel("", "dual y-axis figures are usually terrible"),
  sidebarPanel(
    tabPanel("Axes", 
      sliderInput("left", "Left Axis:", min = 0, max = 400, value = c(0,200), step = 1),
      sliderInput("right", "Right Axis:", min = 0, max = 400, value = c(0,200), step = 1)
    ),
    tableOutput('datatable')
  ),
  mainPanel(
    tabPanel("plot", plotOutput("plot", height = "500px", width = "90%"))
  )
))
