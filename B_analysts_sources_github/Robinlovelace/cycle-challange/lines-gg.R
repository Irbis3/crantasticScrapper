load("shef-flows.RData") # Load data

plot(oas)

proj4string(oas) <- CRS("+init=epsg:27700")

cents <- getSpPPolygonsLabptSlots(oas)
c <- getSpPPolygonsLabptSlots(oas)
cents <- SpatialPointsDataFrame(coords=cents, 
                                data=oas@data, proj4string=CRS("+init=epsg:27700"))
points(cents, col = "Blue")

head(cents@data)
plot(oas, col = "White", border="White")
lines(x=c(c[1,1],c[2,1]), c(c[1,2],c[2,2]), col = "Black")

### Load uni oas
library(rgdal)
uni.oas <- readOGR(dsn=".", "oas-uni-dominated")
plot(uni.oas)
uni.gg <- fortify(uni.oas)
p <- ggplot(data = uni.gg, aes(x = long, y = lat))
p + geom_polygon()

plot(oas, col = "White", border="Black")
for(j in which(oas$ZONE_CODE %in% uni.oas$ZONE_CODE)){
  for(i in which(cents$ZONE_CODE %in% 
    shef.flow$from[which(shef.flow$to == cents$ZONE_CODE[j])])){
    lines(x=c(c[j,1],c[i,1]), c(c[j,2],c[i,2]), col = "Red", lwd=0.1)
  }} # Lines much thinner...

# Plot commuters travelling to most uni-dominated OA
plot(oas, col = "White", border="Black")
for(j in which(oas$ZONE_CODE == "00CGFX0055")){
  for(i in which(cents$ZONE_CODE %in% 
    shef.flow$from[which(shef.flow$to == cents$ZONE_CODE[j])])){
    lines(x=c(c[j,1],c[i,1]), c(c[j,2],c[i,2]), col = "Red", lwd=0.1)
  }} # Lines much thinner...

