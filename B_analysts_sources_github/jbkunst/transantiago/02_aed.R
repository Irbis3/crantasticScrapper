# packages ----------------------------------------------------------------
rm(list = ls())
library(tidyverse)
library(stringr)
library(lubridate)
library(ggmap)

options(digits = 15)

# data --------------------------------------------------------------------
data <- readRDS("data/day-rds/20170509.rds")

data

count(data, nombre, sort = TRUE)

data %>% 
  filter(str_detect(nombre, "504")) %>% 
  distinct(patente, .keep_all = TRUE)

data2 <- data %>% 
  filter(patente == "BDXR-40") %>%
  arrange(fecha_hora)

bbox <- make_bbox(data2$lon, data2$lat, f = 0.1)
map <- get_map(bbox)

ggmap(map) +
  geom_path(data = data2, aes(lon, lat, color = velocidad + 1)) 

data2 %>% 
  filter(velocidad > 0) %>% 
  ggplot() +
  geom_point(aes(fecha_hora, velocidad), alpha = 0.5) + 
  geom_smooth(aes(fecha_hora, velocidad))

ggplot(data2) + 
  geom_path(aes(lon, lat)) +
  facet_wrap(~sentido)

data2 %>% 
  filter(velocidad > 0) %>%
  ggplot() +
  geom_histogram(aes(velocidad))

table(date(data2$fecha_hora))

data2 <- data2 %>% 
  mutate(big_d = ifelse(d > quantile(data2$d, na.rm = TRUE, p = 0.90), 1, 0),
         big_d = ifelse(is.na(big_d), 0, big_d),
         viaje = cumsum(big_d)) 

count(data2, viaje, sort = TRUE)

data2 %>% 
  filter(viaje == 21) %>% 
  View()

data2 %>% 
  filter(viaje %in% c(21,139,99,86,107,37)) %>% 
  ggplot() +
  geom_path(aes(lat, lon, color = lubridate::hour(fecha_hora)), group = 1) +
  facet_wrap(~viaje, scales = "free")


