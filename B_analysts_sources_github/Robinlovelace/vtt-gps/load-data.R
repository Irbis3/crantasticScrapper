source("setup.R")
# load shp data
cways = shapefile("data/cycleway.shp")
plot(cways)

# itea data (not public)
itea = shapefile("data/itea.shp")
# png("figures/initial-plot.png")
plot(itea)
names(itea)
plot(cways, col = "red", add = T)
# dir.create("figures")

# focus on 1 route (to practice)
i1 = itea[1,]
mapview(i1)

# load gpx data
# the file name (update as appropriate)
f = "data/2013_10_05_1230__20131005_1230.gpx"
download.file("https://www.openstreetmap.org/trace/1595482/data", f)
(layers = ogrListLayers(f))
tracks = readOGR(f, layer = layers[3])
plot(tracks)

# individual level data
ind = read.csv("data/tagmyday_fullsample.csv", sep = ";")
