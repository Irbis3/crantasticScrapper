library(shiny) # for interactive visuals
library(leaflet) # for leaflet map
library(dplyr)
library(raster)

# load the data

i <- shapefile("ipu.shp")
plot(i)

i <- spTransform(i, CRSobj = CRS("+init=epsg:4326"))

bbox(i)

leaflet() %>% addProviderTiles(provider = "Esri.WorldImagery") %>% addPolygons(data = i)

# what data do we have?
head(i@data)
