library(shiny) # for interactive visuals
library(leaflet) # for leaflet map
library(dplyr)
library(raster)

# load the data

i <- shapefile("ipu.shp")
i <- spTransform(i, CRSobj = CRS("+init=epsg:4326"))

# remove NAs
i <- i[!is.na(i$since2000),]

shinyServer(function(input, output){

  output$map <- renderLeaflet({

    # create selection
    sel <- grepl(pattern = input$yr, x = i@data$since2000)

    # create popup
    sel2 <- !is.na(i@data$comments)

    p <- i@data$Location
    p[sel2] <- paste(p[sel2], i@data$comments[sel2])

    leaflet() %>%
      addProviderTiles(provider = "Esri.WorldImagery") %>%
      addPolygons(data = i[sel,], popup = p)

  })

})




