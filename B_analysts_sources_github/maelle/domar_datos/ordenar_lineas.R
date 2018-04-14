library(dplyr)
load("data/latestData.RData")
print(latestData)

# ordenar por value
latestData <- latestData %>% arrange(value)
print(latestData)

# ordenar por longitude i después por latitude
latestData <- latestData %>% arrange(longitude, latitude)
print(latestData)

# ordenar por longitude del mas grande al mas grande i después por latitude
latestData <- latestData %>% arrange(desc(longitude), latitude)
print(latestData)
