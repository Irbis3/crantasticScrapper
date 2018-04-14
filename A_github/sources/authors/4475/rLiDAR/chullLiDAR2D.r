#'2D Convex hull of individual tree LiDAR-derived point cloud 
#'
#'@description Compute and plot the 2D convex hull of individual tree LiDAR-derived point cloud 
#'
#'@usage chullLiDAR2D(xyid)
#'
#'@param xyid A 3-column matrix with the x, y coordinates and points id of the LiDAR point cloud.
#'@return Returns A list with components "chullPolygon" and "chullArea", giving the polygon and area  of the convex hull.  
#'@author Carlos Alberto Silva
#'@references \emph{grDevices} package,see \code{\link[grDevices]{chull}}.
#'@examples
#'
#'# Importing LAS file:
#'LASfile <- system.file("extdata", "LASexample1.las", package="rLiDAR")
#'
#'# Reading LAS file
#'LAS<-readLAS(LASfile,short=TRUE)
#'
#'# Height subsetting the data
#'xyz<-subset(LAS[,1:3],LAS[,3] >= 1.37)
#'
#'# Getting LiDAR clusters
#'set.seed(1)
#'clLAS<-kmeans(xyz, 32)
#'
#'# Set the points id 
#'id<-as.factor(clLAS$cluster)
#'
#'# Set the xyid input
#'xyid<-cbind(xyz[,1:2],id)
#'
#'# Compute the LiDAR convex hull of the clusters 
#'chullTrees<-chullLiDAR2D(xyid)
#'
#'# Plotting the LiDAR convex hull
#'library(sp)
#'plot(SpatialPoints(xyid[,1:2]),cex=0.5,col=xyid[,3])
#'plot(chullTrees$chullPolygon,add=TRUE, border='green')
#'
#'# Get the ground-projected area of LiDAR convex hull
#'chullList<-chullTrees$chullArea 
#'summary(chullList) # summary 
#'@importFrom plyr dlply
#'@importFrom sp SpatialPolygons SpatialPolygonsDataFrame rbind.SpatialPolygonsDataFrame
#'@importFrom grDevices chull
#'@export
chullLiDAR2D<-function(xyid) { 
  
  spdfList<-plyr::dlply(as.data.frame(xyid),"id", function(input) {
    cat (".");utils::flush.console()
    ch <- grDevices::chull(input)
    coords <- input[c(ch, ch[1]),-3]
    sp_poly <- sp::SpatialPolygons(list(sp::Polygons(list(sp::Polygon(coords)),ID = input[1,3])))
    spdf <- sp::SpatialPolygonsDataFrame(sp_poly, data = data.frame(input[1,3], row.names = row.names(sp_poly)))
    
  })
  
  spdf = do.call(sp::rbind.SpatialPolygonsDataFrame, spdfList)
  spdf@data$GPA<-as.numeric(sapply(methods::slot(spdf, "polygons"), methods::slot,"area"))
  colnames(spdf@data) <- c("TreeID", "GPA")
  return(list(chullPolygon = spdf, chullArea = spdf@data))
}

