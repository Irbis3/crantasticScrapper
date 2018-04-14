library(dplyr)
load("data/latestData.RData")
print(latestData)

# I que hago yo si quiero tener lastUpdated y value al principio?
latestData <- latestData %>% select(lastUpdated, value, everything())

# Qué pasa si no utilizo everything?
latestData %>% select(lastUpdated, value)
