library(dplyr)
load("data/latestData.RData")

# Qué hay aqui dientro?

glimpse(latestData)

# COLUMNAS
# Tal vez no m'interessa algunas columnas
latestData <- latestData %>% select(- cityURL, - locationURL, - longitude, - latitude)
# Nueva variable con letras minúsculas
latestData <- latestData %>% mutate(smallCountry = tolower(country))
# Nueva variable 
latestData <- latestData %>% mutate(biggerValue = value + 10)
