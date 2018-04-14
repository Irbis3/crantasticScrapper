library(ggplot2)
library(Cairo)   # For nicer ggplot2 output when deployed on Linux

diamonds2 <- diamonds[sample(1:nrow(diamonds), 10000), ]

ui <- fluidPage(
  fluidRow(
    column(width = 6,
      plotOutput("plot1", height = 300,
        brush = "brush"
      )
    ),
    column(width = 6,
      plotOutput("plot2", height = 300)
    )
  )
)

server <- function(input, output) {

  output$plot1 <- renderPlot({
    ggplot(diamonds2, aes(x = cut, y = color)) +
      geom_count() +
      theme_bw()
  })

  brushed <- reactive({
    brushedPoints(diamonds2, input$brush, xvar = "cut", yvar = "color")
  })
  
  output$plot2 <- renderPlot({
    ggplot(diamonds2, aes(carat, price)) +
      geom_point() +
      geom_point(data = brushed(), 
                 color = "green") +
      theme_bw()
  })
}

shinyApp(ui, server)