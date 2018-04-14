library(sp)
df <- read.csv("data/df.csv")
head(df$date)
library(lubridate)
df$time <- mdy(df$date)
cds <- as.matrix(cbind(df$lonWGS84, df$latWGS84))
d <- SpatialPointsDataFrame(cds, data = df)

source("R/load-gs.R")
sel <- is.na(pred$lonWGS84)
pred[sel,] # NAs
pred <- pred[!sel,]
pred$time <- mdy(pred$date)
plot(pred$time)
dpredm <- as.matrix(cbind(pred$lonWGS84, pred$latWGS84))

pred <- as.data.frame(pred)
dpred <- SpatialPointsDataFrame(dpredm, pred)

