# ws ----------------------------------------------------------------------
rm(list = ls())
source("code/99_helpers.R")
library(tidyverse)
library(stringr)
library(viridis)
library(highcharter)
library(janitor)

# data --------------------------------------------------------------------
data <- readRDS("input/data_load.rds")
glimpse(data)

# clustering --------------------------------------------------------------
data <- mutate(data,
               latitude_out = is_out(latitude),
               longitude_out = is_out(longitude)
               )

data %>% 
  filter(!(latitude_out & longitude_out)) %>% 
  sample_frac(0.1) %>% 
  ggplot() + 
  geom_point(aes(latitude, longitude), alpha = 0.5)

datalatlon <- data %>% 
  filter(!(latitude_out & longitude_out)) %>% 
  select(latitude, longitude)

ks <- seq(5, 80, by = 5)
ss <- map_dbl(ks, function(k){ # k <- 2
  message("using k: ", k)
  set.seed(123)
  kmsres <- kmeans(datalatlon, k, iter.max = 20, trace = 1)
  1 - kmsres$betweenss/kmsres$totss
  # kmsres <- kGmedian(dkms, k)
  # sum(kmsres$withinsrs)
})  

hchart(data_frame(x = ks, y = ss), "line")

set.seed(123)
km_clust <- kmeans(datalatlon, 20)
hchart(as.character(km_clust$cluster))

datalatlon <- mutate(datalatlon, lat_lon_cluster = km_clust$cluster)

data <- left_join(ungroup(data), ungroup(datalatlon))
data <- distinct(data, id, .keep_all = TRUE)
data <- mutate(data, lat_lon_cluster = ifelse(is.na(lat_lon_cluster), 0, lat_lon_cluster))


count(data, lat_lon_cluster, sort = TRUE)
count(datalatlon, lat_lon_cluster, sort = TRUE)

# absolute values
data <- data %>% 
  mutate(
    lat_rank = get_rank(latitude),
    lon_rank = get_rank(longitude),
    lat_lon_distance_from_ct = 
      (latitude - median(latitude))^2 +  (longitude - median(longitude))^2,
    lat_dif = latitude - median(latitude),
    lon_dif = longitude - median(longitude),
    lat_dif_sin = sin(lat_dif),
    lon_dif_sin = sin(lon_dif),
    lat_lon_angle = atan2(lat_dif, lon_dif) * (180 / pi)
    )

# cluster values
data <- data %>% 
  group_by(lat_lon_cluster) %>% 
  mutate(
    lat_rank_cl = get_rank(latitude),
    lon_rank_cl = get_rank(longitude),
    lat_lon_distance_from_cl = 
      (latitude - median(latitude))^2 + 
      (longitude - median(longitude))^2,
    lat_dif_cl = latitude - median(latitude),
    lon_dif_cl = longitude - median(longitude),
    lat_dif_sin_cl = sin(lat_dif_cl),
    lon_dif_sin_cl = sin(lon_dif_cl),
    lat_lon_angle_cl = atan2(lat_dif_cl, lon_dif_cl) * (180 / pi)
    ) %>% 
  ungroup()


# mix
data <- data %>% 
  mutate(
    lat_rank_ct_cl = lat_rank_cl/lat_rank,
    lon_rank_ct_cl = lon_rank_cl/lon_rank,
    lat_lon_distance_ct_cl = lat_lon_distance_from_cl/lat_lon_distance_from_ct
  )

data %>% 
  filter(!(latitude_out & longitude_out)) %>% 
  sample_frac(0.1) %>% 
  ggplot() + 
  geom_point(aes(lat_rank_ct_cl, lon_rank_ct_cl,
                 color = factor(lat_lon_cluster)), alpha = 0.5) + 
  scale_color_viridis(discrete = TRUE) +
  scale_x_log10() +
  scale_y_log10()

data %>% 
  filter(!(latitude_out & longitude_out)) %>% 
  sample_frac(0.1) %>% 
  ggplot() + 
  geom_point(aes(latitude, longitude,
                 color = factor(lat_lon_cluster)), alpha = 0.5) + 
  scale_color_viridis(discrete = TRUE)

data %>% 
  filter(!is.na(interest_level)) %>% 
  filter(!(latitude_out & longitude_out)) %>% 
  sample_frac(0.1) %>% 
  ggplot() + 
  geom_point(aes(latitude, longitude,
                 color = factor(interest_level)), alpha = 0.5) + 
  scale_color_viridis(discrete = TRUE)

data %>% 
  filter(!is.na(interest_level)) %>% 
  filter(!(latitude_out & longitude_out)) %>% 
  sample_frac(0.1) %>% 
  ggplot() + 
  geom_point(aes(lat_lon_angle, lat_lon_angle_cl,
                 color = factor(interest_level)), alpha = 0.5) + 
  scale_color_viridis(discrete = TRUE)




# mcdonalds ---------------------------------------------------------------
library(httr)
mcd_url <- "https://www.mcdonalds.com/services/mcd/us/restaurantLocator?latitude=40.7127837&longitude=-74.00594130000002&radius=32180&maxResults=30&country=us&language=en-us"

mcd <- httr::GET(mcd_url) %>% 
  httr::content()

dmcd <- map_df(mcd$features, function(x){
  print(x$properties$addressLine1)
  # x <<- x
  dout <- bind_cols(
    as.data.frame(x$properties$addressLine1),
    as.data.frame(x$geometry)
  ) %>% 
    setNames(c("name", "lon", "lat")) %>% 
    as_data_frame()
  dout
  
})
dmcd <- as_tibble(dmcd)
names(dmcd) <- paste0("mcd_", names(dmcd))

dmcd2 <- map2_df(data$latitude, data$longitude, function(lat, lon){ #lat <- 40.7108; lon <- 73.9539
  dmcd %>% 
    mutate(
      lat_lon_distance_from_mcd =
        (lat - mcd_lat)^2 +
        (lon - mcd_lon)^2) %>% 
    arrange(lat_lon_distance_from_mcd) %>%
    head(1) %>% 
    select(mcd_name, lat_lon_distance_from_mcd)
})

data <- bind_cols(data, dmcd2)
rm(dmcd, dmcd2)

glimpse(data)

# subway ------------------------------------------------------------------
dsubway <- read_tsv("https://data.ny.gov/api/views/i9wp-a4ja/rows.tsv?accessType=DOWNLOAD")
dsubway <- clean_names(dsubway)

glimpse(dsubway)

dsubway <- select(dsubway, division, line, station_name, station_latitude, station_longitude)
dsubway <- distinct(dsubway)
names(dsubway) <- paste0("sw_", names(dsubway))

glimpse(dsubway)
glimpse(data)

dsubway2 <- map2_df(data$latitude, data$longitude, function(lat, lon){ #lat <- 40.7108; lon <- 73.9539
  
  dsubway %>% 
    mutate(
      lat_lon_distance_from_sw =
        (lat - sw_station_latitude)^2 +
        (lon - sw_station_longitude)^2) %>% 
    arrange(lat_lon_distance_from_sw) %>%
    head(1) %>% 
    select(-sw_station_latitude, -sw_station_longitude)
})

data <- bind_cols(data, dsubway2)
rm(dsubway, dsubway2)


# macstore ----------------------------------------------------------------
library(rvest)
library(ggmap)

url_base <- "http://www.apple.com/"
url_stores <- read_html(file.path(url_base, "retail/storelist/")) %>% 
  html_node("#usstores > div > div > div:nth-child(2) > div:nth-child(20) > ul") %>%
  html_nodes("a") %>% 
  html_attr("href")

dmacstores <- map_df(url_stores, function(url){ # url <- sample(url_stores, 1)
  message(url)
  store_street <- file.path(url_base, url) %>% 
    read_html() %>% 
    html_node(".store-street") %>% 
    html_text()
  message(store_street)
  
  dout <- data_frame(name = basename(url)) %>% 
    cbind(geocode(paste0(store_street, ", New York"))) 
  
  names(dout) <- paste0("macstore_", names(dout))
    
  dout
  
})

dmacstores2 <- map2_df(data$latitude, data$longitude, function(lat, lon){ # lat <- 40.7108; lon <- 73.9539
  
  dmacstores %>% 
    mutate(
      lat_lon_distance_from_mac =
        (lat - macstore_lat)^2 +
        (lon - macstore_lon)^2) %>% 
    arrange(lat_lon_distance_from_mac) %>%
    head(1) %>% 
    select(macstore_name, lat_lon_distance_from_mac)
})

data <- bind_cols(data, dmacstores2)
rm(dmacstores, dmacstores2, url_base, url_stores)

glimpse(data)

# export ------------------------------------------------------------------
saveRDS(data, "input/data_feat_lat_lon.rds")
glimpse(data)


