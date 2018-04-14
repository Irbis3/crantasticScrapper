
# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)
library(datasets)
data(airquality)

shinyServer(function(input, output) {
        output$loessPlot <- renderPlot({
                span <- input$span
                range <- input$range
                month <- input$month
                aqmonth <- subset(airquality, Month == month)
                aqother <- subset(airquality, Month != month)
                with(airquality, {
                        plot(Temp, Ozone, xlim = range, 
                             type = "n", main = input$title)
                })
                with(aqother, points(Temp, Ozone))
                with(aqmonth, {
                        points(Temp, Ozone, col = "blue", 
                               pch = 19)
                })
                with(airquality, {
                        fit <- loess.smooth(Temp, Ozone, 
                                            span = span,
                                            evaluation = 200)
                        lines(fit)
                })
        })
})
