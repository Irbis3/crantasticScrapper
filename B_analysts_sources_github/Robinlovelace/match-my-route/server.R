
# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)
library(leaflet)

r <- readRDS("r.Rds")
rf <- readRDS("rf.Rds")

shinyServer(function(input, output) {

  output$map <- renderLeaflet({

    leaflet() %>% addTiles() %>% addPolylines(data = r, color = "red")

  })

  output$map2 <- renderLeaflet({

    leaflet() %>% addTiles() %>% addPolylines(data = rf) %>%
      addPolylines(data = r, stroke = 3, color = "red")

  })

})
