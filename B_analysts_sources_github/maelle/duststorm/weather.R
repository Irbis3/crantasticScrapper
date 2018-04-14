library("riem")
library("dplyr")
library("lubridate")
library("tidyr")
airports <- readr::read_csv("data/airports.csv")
india_airports <- riem_stations(network = "IN__ASOS")
india_airports <- filter(india_airports, id %in% airports$airport)
save(india_airports, file = "data/india_airports.RData")

weather <- riem_measures(station = "VIDP", date_start = "2010-01-01",
                        date_end = "2016-06-01")
  

save(weather, file = "data/weather.RData")