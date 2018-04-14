# ws ----------------------------------------------------------------------
rm(list = ls())
source("code/99_helpers.R")
library(tidyverse)
library(stringr)
library(highcharter)

# data --------------------------------------------------------------------
data <- readRDS("input/data_load.rds")
glimpse(data)


# price rank --------------------------------------------------------------
data <- mutate(data, price_rank = get_rank(price))

# price lat lot -----------------------------------------------------------

hchart(as.character(data$lat_lon_cluster))

data <- data %>% 
  group_by(lat_lon_cluster) %>% 
  mutate(price_rank_lat_lon_cluster = get_rank(price)) %>% 
  ungroup()
  
data %>% 
  sample_frac(0.2) %>% 
  ggplot() + 
  geom_point(aes(price_rank_lat_lon_cluster, price, 
                 color = factor(lat_lon_cluster)))

data %>% 
  filter(!(latitude_out & longitude_out)) %>% 
  sample_frac(0.2) %>% 
  ggplot() + 
  geom_point(aes(latitude, longitude, color = factor(lat_lon_cluster)),
                 alpha = 0.5)


glimpse(data)
# asd ---------------------------------------------------------------------
quantile(data$bathrooms/data$bedrooms, 0:10/10, na.rm = TRUE)

