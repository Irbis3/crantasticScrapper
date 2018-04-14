library(shiny)
library(leaflet)
library(sp)
shinyServer(function(input, output) {
  
  p_tt = readRDS("data/p_tt.Rds")
  track_tt = readRDS("data/track_tt.Rds")
  
  p_viz = reactive({
    p_tt[p_tt$ele > input$elevation[1] & p_tt$ele < input$elevation[2],]
  })

  output$map <- renderLeaflet({
    # mapview(track_tt) + mapview(p_viz())
    leaflet() %>%
      addTiles() %>%
      addPolylines(data = track_tt) %>%
      addCircleMarkers(data = p_viz())
  })
})
