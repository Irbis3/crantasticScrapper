# Aim: create basic geospatial vector data classes:
# points, lines, polygons
# Building on Barry Rowlingson's cheatsheet:
# http://www.maths.lancs.ac.uk/~rowlings/Teaching/UseR2012/cheatsheet.html

# load the sp spatial data class package
library(sp)

# Create point data
x <- c(-1, 1, 1, -1)
y <- c(1, 1, -1, -1)
plot(x, y)
coords <- cbind(x, y)
sp <- SpatialPoints(coords)
plot(sp)
data <- data.frame(v1 = 1:length(sp))
spdf <- SpatialPointsDataFrame(sp, data)

# Create line data
c1 = cbind(x[1:2], y[1:2])
c2 = cbind(x[2:3], y[2:3])
c3 = cbind(x[1:3], y[1:3])

# simple line strings
L1 = Line(c1)
L2 = Line(c2)
L3 = Line(c3)

# single/multiple line strings
Ls1 = Lines(list(L1), ID = "a")
Ls2 = Lines(list(L2, L3), ID = "b")

# with spatial nature
SL1 = SpatialLines(list(Ls1))
SL12 = SpatialLines(list(Ls1, Ls2))

plot(SL1)
plot(SL12)

# made into spatial data frame
SLDF = SpatialLinesDataFrame(SL12,
                             data.frame(Z = c("Road", "River"),
                                        row.names = c("a", "b")))
# create polygon
P1 <- Polygon(coords)
Ps1 <- Polygons(list(P1), ID = 1)
SP <- SpatialPolygons(list(Ps1))
plot(SP, col = "black")

SPDF <- SpatialPolygonsDataFrame(SP,
                                 data.frame(N = c("one")))

# flourish at the end with a rotation (spatstat is an alternative here)
SP1 <- maptools::elide(SP, rotate = 15, center = c(0,0))
plot(SP1, add = T)

# generate output of this work
knitr::spin("spatial-basics.R")
