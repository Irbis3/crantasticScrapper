library(dplyr)
load("data/latestData.RData")


# LINEAS
# O si quiero solo los datos (values) positivos
latestPositivos <- latestData %>% filter(value > 0)
# O solo las datos para Mongolia
latestMN <- latestData %>% filter(country == "MN")
# O solo las datos positivos para Mongolia
latestPositivosMN <- latestData %>% filter(country == "MN", value > 0)

# between
latestData %>% filter(between(value, 0, 20))

# %in%
latestData %>% filter(city %in% c("Ulaanbaatar", "Farmington"))
