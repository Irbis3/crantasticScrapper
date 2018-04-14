# Aim: read in travelAI data

library(jsonlite)

obj = jsonlite::fromJSON("~/Dropbox/TravelAi_Example_travels Oct-Dec15.geojson")
class(obj) # it's a data frame!
length(obj)
names(obj) #  what are we looking at?
sapply(obj, class) # a range of variable classes
# View(obj) 

# Make coordinates numeric and columns
library(stringr)
obj$to_locx <- sapply(obj$to_loc, `[[`, 1)
obj$to_locy <- sapply(obj$to_loc, `[[`, 2)
obj$from_locx <- sapply(obj$from_loc, `[[`, 1)
obj$from_locy <- sapply(obj$from_loc, `[[`, 2)
plot(obj$from_locx, obj$from_locy) # what are we dealing with?

# Make the data a spatial class
library(sp)
p <- SpatialPointsDataFrame(coords = obj[c("from_locx", "from_locy")], data = obj)
library(tmap)
b <- bb("London")
e = b
e <- rgeos::readWKT(paste('POLYGON((',e[1,1],e[2,1],',',e[1,1],e[2,2],
      ',',e[1,2],e[2,2],',',e[1,2],e[2,1],',',e[1,1],e[2,1],'))',collapse=' '))
p_lnd <- p[e,]

plot(p_lnd)

knitr::spin("catch/import-json.R")

# Test code (not working)

# library(geojsonio)
# obj = geojson_read("~/Dropbox/TravelAi_Example_travels Oct-Dec15.geojson") # fails