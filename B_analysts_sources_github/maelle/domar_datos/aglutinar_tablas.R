library("dplyr")
load("data/latestData.RData")
load("data/countries.RData")
load("data/moreData.RData")

# anadir lineas
bind_rows(latestData, moreData)
# si no hay las mismas columnas
latestData2 <- latestData %>% select(location, city)
bind_rows(latestData2, moreData)

# anadir el nombre de los paisos
glimpse(latestData)
glimpse(countries)

latestData %>% left_join(countries, by = c("country" = "code"))

latestData %>% left_join(countries, by = c("country" = "code")) %>%
  mutate(country = name) %>%
  select(- name)
