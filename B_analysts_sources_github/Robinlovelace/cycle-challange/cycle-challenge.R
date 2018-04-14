# allflow <- read.csv("/media/3C3863A438635BC0/MyFolder/Datagis/flowdata/OA-flows/W301_OUT.csv")
# names(allflow) <- c("from", "to", "n")

### Load uni oas
library(rgdal)
uni.oas <- readOGR(dsn=".", "oas-uni-dominated")
to.uni <- allflow[which(allflow$to %in% uni.oas$ZONE_CODE),1:3]

### Load all oas
alloas <- read.csv("/media/3C3863A438635BC0/MyFolder/Datagis/England/oas/alloas_ttw.csv")
head(alloas)
o <- alloas[which(alloas$Zone.Code %in% to.uni$from),] # Origin dataset
head(o)
names(o) <- c("zc", "area", "x", "y", "all")

### load oas
shef.oas <- readOGR(".", "oas")
plot(shef.oas)
### Tidy up, remove massive datasets
rm(allflow, alloas, origin)
# save.image("input.RData")

### Prepare and run lines plot - this works!
load("input.RData")
c <- getSpPPolygonsLabptSlots(uni.oas)
to.uni$from <- as.character(to.uni$from)
plot(shef.oas, col = "White", border="Black")
for(j in 1:nrow(to.uni)){
  lines(c(o$x[which(o$zc == to.uni$from[j])], c[1,1]),
        c(o$y[which(o$zc == to.uni$from[j])], c[1,2]),
        col = "Red", lwd = 0.05)
}

for(j in 1:nrow(to.uni)){
  plot(c(o$x[which(o$zc == to.uni$to[j])], c[1,1]))
}


# Try just to "00CGFX0055"
to.unic <- to.uni[which(to.uni$to == "00CGFX0055"),]

summary(to.unic)
to.unic$colour <- "Red"
to.unic$colour[which(to.unic$n > 3)] <- "Blue"
to.unic$colour[which(to.unic$n > 6)] <- "Green"


to.unic$lwd <- 0.4
to.unic$lwd[which(to.unic$n > 3)] <- 1
to.unic$lwd[which(to.unic$n > 6)] <- 2
to.unic$n[which(to.unic$n > 6)]

plot(shef.oas, col = "White", border="Black")

iis <- order(to.unic$n)

for(i in iis){
  to.unic$dis[i] <- sqrt((o$x[which(o$zc == to.unic$from[i])] - c[4,1])^2 + (o$y[which(o$zc == to.unic$from[i])]- c[4,2])^2 )
  lines(c(o$x[which(o$zc == to.unic$from[i])], c[4,1]),
        c(o$y[which(o$zc == to.unic$from[i])], c[4,2]),
        col = to.unic$colour[i], lwd = to.unic$lwd[i])
}

summary(to.unic$dis)
distances <- to.unic$dis
distances <- c(distances, to.unic$dis[which(to.unic$n > 3)])
distances <- c(distances, to.unic$dis[which(to.unic$n > 6)])
distances <- c(distances, distances, distances[sample(distances, 300)])
summary(distances)
distances <- distances/1000
length(which(distances < 8)) / length(distances)
length(which(distances < 5.71)) / length(distances)


d2 <- distances[which(distances <= 50)]
hist(d2, main= "", xlab = "Euclidean distance (km)", freq=T)
summary(d2)

h <- ggplot(y=d2 x=nrow())
h + geom_histogram()

### How many origins are in sheffield
l <- which(to.unic$from %in% shef.oas$ZONE_CODE)
length(l) - nrow(to.unic)

# Sheffield cycling circuity
download.file("https://github.com/npct/pct-data/raw/master/Sheffield/rf.Rds", "rf.Rds", mode = "wb")
download.file("https://github.com/npct/pct-data/raw/master/Sheffield/rq.Rds", "rq.Rds", mode = "wb")
download.file("https://github.com/npct/pct-data/raw/master/Sheffield/l.Rds", "l.Rds", mode = "wb")


rf <- readRDS("rf.Rds")
l <- readRDS("l.Rds")

names(l)

library(dplyr)
df <- l@data[c("dist", "dist_fast",     "dist_quiet")]
names(df) <- c("Euclidean distance (km)", "'Fastest' route distance", "'Quietest' route distance")
df[2:3] <- df[2:3] * 1000

plot(df)
abline(v = 2)
cor(l@data[c("dist", "dist_fast",     "dist_quiet")])

pairs(df, panel= function(x,y,...){
  points(x,y);
  abline(coef = c(0,1))})

summary(df[2] / df[1])
