# Get zones
library(sf)
library(tmap)
library(stplanr)
tmap_mode("view")

zones <- readRDS("../travelHack/PCT/msoa.Rds")
zones <- st_as_sf(zones)
zones <- zones[,c("geo_code","avslope","geo_label","all","light_rail","train","bus","taxi","motorbike","car_driver","car_passenger","bicycle","foot","other")]

bus.stops <- st_read("../travelHack/osm/bus_stops_points.shp")
bike.parking <- st_read("../travelHack/osm/bike_parks_points.shp")
lines <- readRDS("../travelHack/listofroutes.Rds")

bus.stops <- st_transform(bus.stops, 27700)
bike.parking <- st_transform(bike.parking, 27700)

lines.buff <- st_buffer(lines, 3000)
lines.buff <- st_union(lines.buff, by_feature = FALSE)
#qtm(lines.buff)


od <- line_to_points(lines)
st_crs(od) <- 27700
#qtm(od)

od.buff <- st_buffer(od, 3000)
od.buff <- st_union(od.buff, by_feature = FALSE)
bus.stops <- bus.stops[od.buff,]


#qtm(bus.stops) +
 # qtm(zones, fill = "bus")

#qtm(od, dots.col = "red") +
lines$weight <- lines$all / 100

qtm(zones) +
qtm(bus.stops, dots.col = "black") +
qtm(bike.parking, dots.col = "green") +
qtm(lines, lines.lwd = 4)  

