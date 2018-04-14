
# This is the user-interface definition of a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)
library(datasets)
data(airquality)

shinyUI(fluidPage(
        ## App title
        titlePanel("Ozone and Temperature in New York"),
        
        sidebarLayout(
                sidebarPanel(
                        ## Slider for the span of the loess
                        sliderInput("span",
                                    "Span for Smoother:",
                                    min = 0.1,
                                    max = 0.9,
                                    value = 2/3
                        ),
                        ## Double slider for temperature range
                        sliderInput("range",
                                    "Select Range:",
                                    min = min(airquality$Temp),
                                    max = max(airquality$Temp),
                                    value = c(min(airquality$Temp), 
                                              max(airquality$Temp))
                        ),
                        ## Highlight a month in blue
                        selectInput("month",
                                    "Show Month:",
                                    choices = list("May" = 5, "June" = 6, 
                                                   "July" = 7, "August" = 8, 
                                                   "September" = 9, 
                                                   selected = 7)
                        ),
                        ## Enter a custop plot title
                        textInput("title", "Enter Plot Title")
                ),
                
                ## Show the generated scatterplot
                mainPanel(
                        plotOutput("loessPlot")
                )
        )
))







#sliderInput("month",
#            "Select Month:",
#            min = 5,
#            max = 9,
#            step = 1,
#            value = 7
#),
