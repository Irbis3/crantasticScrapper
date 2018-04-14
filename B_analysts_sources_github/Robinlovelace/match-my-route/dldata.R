# generate data for the app

library(stplanr)
r <- cyclestreet_route()
saveRDS(r, file = "r.Rds")

rf <- readRDS("~/repos/pct/pct-data/manchester/rf.Rds")
summary(rf)
proj4string(r) <- proj4string(rf)
rf <- rf[r,]
plot(rf)

saveRDS(rf, "rf.Rds")
