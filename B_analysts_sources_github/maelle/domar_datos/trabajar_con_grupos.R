library(dplyr)
load("data/latestData.RData")

# en cada pais, orden de value
latestData %>%
  group_by(country) %>% 
  mutate(rankingValue = rank(value)) %>%
  select(location, city, country, value, rankingValue) %>%
  arrange(country) %>%
  ungroup()

# ahora tal vez quiero ver la secunda "location" en cada pais
latestData %>%
  group_by(country) %>% 
  mutate(rankingValue = rank(value)) %>%
  select(location, city, country, value, rankingValue) %>%
  arrange(country) %>%
  filter(rankingValue == 2)