require(rgeos)

# CREATE SOME DATA USING meuse DATASET
data(meuse)
coordinates(meuse) <- ~x+y
pts <- meuse[sample(1:dim(meuse)[1],142),]  
data(meuse.grid) 
coordinates(meuse.grid) = c("x", "y") 
gridded(meuse.grid) <- TRUE 
meuse.grid[["idist"]] = 1 - meuse.grid[["dist"]]    
polys <- as(meuse.grid, "SpatialPolygonsDataFrame")
polys <- polys[sample(1:dim(polys)[1],10),]   
plot(polys)
plot(pts,pch=19,cex=1.25,add=TRUE)      

# LOOP USING gDistance, DISTANCES STORED IN LIST OBJECT
Fdist <- list()
for(i in 1:dim(pts)[1]) {
  pDist <- vector()
  for(j in 1:dim(polys)[1]) { 
    pDist <- append(pDist, gDistance(pts[i,],polys[j,])) 
  }
  Fdist[[i]] <- pDist
} 

# RETURN POLYGON (NUMBER) WITH THE SMALLEST DISTANCE FOR EACH POINT  
( min.dist <- unlist(lapply(Fdist, FUN=function(x) which(x == min(x))[1])) ) 

# RETURN DISTANCE TO NEAREST POLYGON
( PolyDist <- unlist(lapply(Fdist, FUN=function(x) min(x)[1])) ) 

# CREATE POLYGON-ID AND MINIMUM DISTANCE COLUMNS IN POINT FEATURE CLASS
pts@data <- data.frame(pts@data, PolyID=min.dist, PDist=PolyDist)

# PLOT RESULTS
require(classInt)
( cuts <- classIntervals(pts@data$PDist, 10, style="quantile") )
plotclr <- colorRampPalette(c("cyan", "yellow", "red"))( 20 )
colcode <- findColours(cuts, plotclr)
plot(polys,col="black")
plot(pts, col=colcode, pch=19, add=TRUE)



library(sp)
x = cbind(c(0,1,1,0,0),c(0,0,1,1,0))
pol = SpatialPolygons(list(Polygons(list(Polygon(x)), "ID")))
#random points in unit square:
  
  set.seed(131)
n = 50000
pts = SpatialPoints(cbind(runif(n), runif(n)))
plot(pol)
points(pts, col = 'red')
#compute distances:
  
library(rgeos)
gd <- as.vector(gDistance(pts, as(pol, "SpatialLines"), byid = TRUE)) # dist to line
#add to plot:
  
text(coordinates(pts),
       as.character(
         round(gd, 3)),
       pos = 4)

hist(gd)
