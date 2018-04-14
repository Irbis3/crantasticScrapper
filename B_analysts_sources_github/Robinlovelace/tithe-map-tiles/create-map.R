library(sp)
tmp = raster::shapefile("TMparcels_simplified.shp")
proj4string(tmp) = CRS("+init=epsg:27700")
tmp = spTransform(tmp, CRSobj = CRS("+init=epsg:4326"))

# Mapview way:
mapview::mapview(tmp)

# The tmap way:
library(tmap)
qtm(tmp, fill = "LandUse")
tmap_mode("view")
qtm(tmp, fill = "LandUse")

# The leaflet way
library(leaflet)
leaflet() %>% addTiles() %>% addPolygons(data = tmp)
pal = colorFactor(palette = "Blues", domain = tmp$LandUse)
tmp$lab = knitr::kable(tmp@data)
i = 1
for(i in nrow(tmp)){
  df = data.frame(Owner)
  tmp$lab = knitr::kable()
}

tmp_mini = tmp
tmp_mini@data = tmp@data[,c(1, 2, 3, 4, 5)]
lab_list = brewtable_mod(tmp_mini)
i = 1
for(i in 1:nrow(tmp)){
  df_tab = data.frame(var = c("Owner", "Occupier", "Field name", "Land use"),
                      value = c(tmp$Owner[i], tmp$Occupier[i], tmp$FieldName[i], tmp$LandUse[i]))
  # df_html = knitr::kable(df_tab, format = "html", row.names = F) # limited control
  # df_html = xtable::xtable(df_tab) # latex
  df_html = htmlTable::htmlTable(df_tab)
  tab_list[[i]] = df_html
}



leaflet() %>% addTiles() %>%
  addPolygons(data = tmp, weight = 1, color = "#000000", popup = lab_list,
            fillColor= ~ pal(LandUse),
            fillOpacity=0.7,
            
            # label = ~tmp@data,
            # labelOptions = labelOptions(direction = 'auto')
            )

tmp_sf = sf::st_read("TMparcels_simplified.shp")
head(tmp_sf)
