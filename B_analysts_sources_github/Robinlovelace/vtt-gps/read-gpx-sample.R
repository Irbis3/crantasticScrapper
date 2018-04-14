# # Read the data in from it's original source (commented)
# library(rgdal)
# library(raster)
# f = "D:\\london_2015-10-06.gpx/london_2015-10-06.gpx"
# (layers = ogrListLayers(f))
# tracks = readOGR(f, layer = layers[3])
# tracks_mini = tracks[1:100,]
# plot(tracks_mini)
# shapefile(tracks_mini, file = "D://tmp/test-bikecit-data.shp")

# Read the sample data in to test
tracks_mini = shapefile("D://tmp/test-bikecit-data.shp")
names(tracks_mini)
head(tracks_mini)
