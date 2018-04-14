library("ropenaq")
library("dplyr")
india_locs <- aq_locations(limit = 1000, page = 1,
                           city = "Delhi",
                           parameter = "pm10")$results

meas <- NULL

for(loc in india_locs$locationURL){
  print(loc)
  no_measures <- aq_measurements(location = loc,
                                 page = 1,
                                 limit= 1,
                                 parameter = "pm10")$meta$found
  no_pages <- ceiling(no_measures/1000)
  for(page in 1:no_pages){
    print(page)
    meas <- rbind(meas,
                  aq_measurements(location = loc,
                                  page = page,
                                  limit = 1000,
                                  parameter = "pm10")$results)
  }
}
meas25 <- meas
save(meas25, file = "data/pm10.RData")