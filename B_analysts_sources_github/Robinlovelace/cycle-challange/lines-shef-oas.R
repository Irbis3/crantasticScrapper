### Example of flow data: loading, manipulating, visualising

load("shef-flows.RData") # Load data
head(oas@data)

### Plotting choropleth map
library(GISTools)
shades <- shading(breaks=c(50,100,200,500), cols=brewer.pal(5,'Blues'))
choropleth(oas, v=oas$to, shades)
bbox(oas) # Bounding box of map
legend.loc <- c(bbox(oas)[1,1] + 100, bbox(oas)[2,1] + 22000)
choro.legend(legend.loc[1], legend.loc[2], shades) # Looks clunky

### Exploring SpatialPolygonsDataFrame
head(coordinates(oas)) # Looks like OSGB2770
proj4string(oas) 
proj4string(oas) <- CRS("+init=epsg:27700")

cents <- getSpPPolygonsLabptSlots(oas)
c <- getSpPPolygonsLabptSlots(oas)
cents <- SpatialPointsDataFrame(coords=cents, 
            data=oas@data, proj4string=CRS("+init=epsg:27700"))
points(cents, col = "Blue")

head(cents@data)
plot(oas, col = "White", border="White")
lines(x=c(c[1,1],c[2,1]), c(c[1,2],c[2,2]), col = "Black")

### Plot of lines from home centroid to all places it goes to
for(i in 1:1744){
  lines(x=c(c[1,1],c[i,1]), c(c[1,2],c[i,2]), col = "Black")
}

### Plot of lines from one centroid to any others
plot(oas, col = "White", border="White")
for(i in which(cents$ZONE_CODE %in% 
  shef.flow$to[which(shef.flow$from == cents$ZONE_CODE[1])])){
  lines(x=c(c[1,1],c[i,1]), c(c[1,2],c[i,2]), col = "Black")
}
### More general version, with j instead of 1
plot(oas, col = "White", border="Black")
j=1000
for(i in which(cents$ZONE_CODE %in% 
  shef.flow$to[which(shef.flow$from == cents$ZONE_CODE[j])])){
  lines(x=c(c[j,1],c[i,1]), c(c[j,2],c[i,2]), col = "Red")
} # That works

### More general version, with many origins too
plot(oas, col = "White", border="Black")
for(j in which(oas$to > 1000)){
for(i in which(cents$ZONE_CODE %in% 
  shef.flow$to[which(shef.flow$from == cents$ZONE_CODE[j])])){
  lines(x=c(c[j,1],c[i,1]), c(c[j,2],c[i,2]), col = "Red", alpha = 0.5)
}} # That works

### plot centres employing more than 1000 people
for(j in which(oas$to > 1000)){
  points(c[j,1],c[j,2],  col = "Blue")
  } # That works

### Where people come from, who work in emp. centres (more dense?)
# Swap from and to around in second loop
plot(oas, col = "White", border="Black")
for(j in which(oas$to > 1000)){
  for(i in which(cents$ZONE_CODE %in% 
    shef.flow$from[which(shef.flow$to == cents$ZONE_CODE[j])])){
    lines(x=c(c[j,1],c[i,1]), c(c[j,2],c[i,2]), col = "Red", lwd=0.1)
  }} # That works, for over 1000 it's very dense

shades <- shading(breaks=c(50,200,500,1000), cols=brewer.pal(5,'Blues'))
choropleth(oas, v=oas$to, shades)
for(j in which(oas$to > 5000)){
  for(i in which(cents$ZONE_CODE %in% 
    shef.flow$from[which(shef.flow$to == cents$ZONE_CODE[j])])){
    lines(x=c(c[j,1],c[i,1]), c(c[j,2],c[i,2]), col = "Red", lwd=0.1)
  }} # That works, for over 5000 it's better

### plot centres employing more than 1000 people
for(j in which(oas$to > 5000)){
  points(c[j,1],c[j,2],  col = "Blue")
} # That works

### Stats - what prop of work in large employers?
sum(oas$to[which(oas$to > 5000)])/sum(oas$to)     # Prop concentrated in large employers
library(ineq)
Gini(oas$to)
??lorenz
summary(oas$to)
Lc(oas$to, plot=T)
summary(oas$KS0150001)
Lc(oas$KS0150001, plot=T)

sum(shef.flow$to)
sum(oas$KS0150001)

plot(oas, col = "White", border="Black")
for(j in which(oas$to > -1)){
  for(i in which(cents$ZONE_CODE %in% 
    shef.flow$from[which(shef.flow$to == cents$ZONE_CODE[j])])){
    lines(x=c(c[j,1],c[i,1]), c(c[j,2],c[i,2]), col = "Red", lwd=0.01)
  }} # Lines much thinner...

### plot centres employing more than 1000 people
for(j in which(oas$to > 1000)){
  points(c[j,1],c[j,2],  col = "Blue")
} # That works
cents.1000 <- cents[which(oas$to > 1000),]
plot(cents.1000)
writeSpatialShape(cents.1000, "emp-centres-1000")

plot(coordinates(cents[1,]), coordinates(cents)[2,])








