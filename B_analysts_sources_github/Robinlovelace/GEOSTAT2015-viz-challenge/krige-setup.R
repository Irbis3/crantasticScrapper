bb <- bbox(d)
cs <- c(0.5, 0.5) # cell size
cc <- bb[, 1] + (cs/2) #
cd <- ceiling(diff(t(bb))/cs)
lgrd <- GridTopology(cellcentre.offset = cc, cellsize = cs, cells.dim = cd)

gd <- SpatialGrid(grid = lgrd)
gddf <- coordinates(gd)
gddf <- data.frame(gddf)
names(gddf) <- c("x", "y")
gridded(gddf) <- ~x+y