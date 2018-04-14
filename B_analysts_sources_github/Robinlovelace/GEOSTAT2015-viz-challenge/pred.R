# Spatial prediction
library(spatstat)
library(maptools)

source("R/load-gs.R")

?predict.ppm

pppp <- as.ppp(dpred)
pppd <- as.ppp(d)

nn <- nncross(pppp, pppd, what = "which")
length(nn)
nrow(d)
nrow(dpred)

plot(pppp)
plot(d)

plot(d[nn[1:100],])
points(dpred[1:100,])

points(d, col = "red")
points(dpred[ dpred@data$])

df$time[which.min(abs(df$time - dpred@data$time))]

precip_pred <- rep(NA, nrow(dpred))

# prediction 1: 0.07 r value!
for(i in 1:nrow(dpred)){
  nearest_id <- d@data$rdb_id[nn[i]]
  dsub <- d[d@data$rdb_id == nearest_id,]
  precip_pred[i] <- dsub@data$precip_mm[which.min(abs(dpred@data$time[i] - dsub$time))]
}

plot(dpred@data$Jean, precip_pred)
cor(dpred@data$Jean, precip_pred, use = "complete.obs")

# prediction 2: r = 0.45!
for(i in 1:nrow(dpred)){
  nearest_time <- d@data$time[which.min(abs(dpred@data$time[i] - d$time))]
  dsub <- d[d@data$time == nearest_time,]

  dsubp <- as.ppp(dsub)

  nearest_id <- nncross(pppp[i], dsubp, what = "which")

#   # test it works!
#   plot(dsub)
#   plot(dpred[i, ], add = T, col = "red")
#   points(dsub[nearest_id,])

  precip_pred[i] <- dsub@data$precip_mm[nearest_id]
}

pred$robin <- precip_pred

write.csv(pred, "prediction-rl.csv")

plot(dpred@data$Jean, precip_pred)
cor(dpred@data$Jean, precip_pred, use = "complete.obs")

