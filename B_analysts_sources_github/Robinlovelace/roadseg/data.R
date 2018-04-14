# Aim: download data
library(osmplotr)
library(tmap)
library(sp)
b = bb("Leeds", ext = 0.1) # bounding box for subsetting data
bsquare = stplanr::bb2poly(b)
plot(bsquare)
proj4string(bsquare) = CRS("+init=epsg:4326")
# Download data from:
# https://www.ordnancesurvey.co.uk/opendatadownload/products.html
# Issue: this takes ages to load into R
# Solution: use qgis to subset it
# openroads = raster::shapefile("/media/robin/Extra Drive 1/open-roads_1365445/ROAD.shp")
summary(as.factor(openroads$class))
barplot(summary(as.factor(openroads$class)))
# saveRDS(openroads, "/media/robin/Extra Drive 1/open-roads_1365445/openroads.Rds")
open_a = openroads[openroads$class == "A Road",]
bsquare = spTransform(bsquare, proj4string(openroads))
open_a_lds = open_a[bsquare,]
plot(open_a_lds)
plot(bsquare, add = T)
geojsonio::geojson_write(open_a_lds, file = "data/open_a_lds.geojson")

# Download osm data

h = extract_osm_objects(key = "highway", bbox =  b)
summary(h)
h_all = h
h = h$obj
h = spTransform(h, CRS("+init=epsg:27700"))
plot(h)
length(h)
geojsonio::geojson_write(h, file = "data/highways-osm-leeds.geojson")
summary(rgeos::gLength(h, byid = TRUE))
mapview::mapview(h)



