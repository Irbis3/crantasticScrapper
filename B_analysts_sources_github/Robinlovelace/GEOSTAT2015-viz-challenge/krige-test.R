# create raster of kriged points for mean precip
library(raster)
library(gstat)

bb <- bbox(d)
cs <- c(1, 1) # cell size
cc <- bb[, 1] + (cs/2) #
cd <- ceiling(diff(t(bb))/cs)
lgrd <- GridTopology(cellcentre.offset = cc, cellsize = cs, cells.dim = cd)
gd <- SpatialGrid(grid = lgrd)
gddf <- raster(d)
gddf <- coordinates(gd)
gddf <- data.frame(gddf)
names(gddf) <- c("x", "y")
gridded(gddf) <- ~x+y
head(gddf)
plot(gd)

# Find mean for each point
d_uniq <- d[!duplicated(d@data$rdb_id),]
plot(d_uniq)

d_avs <- group_by(df, rdb_id) %>%
  summarise(Mean_pecip = mean(precip_mm))

m <- vgm(.59, "Sph", 874, .04)
kr <- krige(formula = precip_mm ~ 1, locations = d_uniq, newdata = gddf)
r <- raster(kr)
plot(r)

library(rasterVis)
levelplot(r) +
  layer(sp.points(d))

int <- interpolate(object = r, kr)
