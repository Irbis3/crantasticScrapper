library(shiny)
library(shinydashboard)

ui <- dashboardPage(
  dashboardHeader(
    
  ),
  dashboardSidebar(
    sliderInput("nrand", "Simulaciones", min = 50, max = 100, value = 70),
    selectInput("col", "Color", c("red", "blue", "black")),
    checkboxInput("punto", "Puntos:", value = FALSE)
  ),
  dashboardBody(
    fluidRow(box(width = 12, plotOutput("outplot")))
  )
)
  
server <- function(input, output) {
  output$outplot <- renderPlot({
    set.seed(123)
    x <- rnorm(input$nrand)
    t <- ifelse(input$punto, "b", "l")
    plot(x, type = t, col = input$col)
  })
}

shinyApp(ui = ui, server = server)
# runApp("app-04-shinydashboard", launch.browser = TRUE, display.mode = "showcase")
