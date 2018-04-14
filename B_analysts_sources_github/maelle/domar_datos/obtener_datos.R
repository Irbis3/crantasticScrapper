library("Ropenaq")
library("dplyr")

# observaciones mas nuevas
latestData <- aq_latest()

save(latestData, file = "data/latestData.RData")

# todas las observaciones para una estacion de Ulaanbaatar en Mongolia
stationName <- "100+ail"
meas100ail <- NULL
for(page in 1:83){
  print(page)
  meas100ail <- rbind(meas100ail,
                              aq_measurements(location = stationName, limit = 1000,
                                              page = page))
}
save(meas100ail, file = "data/meas100ail.RData")

# countrie names
countries <- aq_countries()
save(countries, file = "data/countries.RData")

# dummy data
moreData <- aq_measurements(limit = 1000)
save(moreData, file = "data/moreData.RData")
