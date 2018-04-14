source("opendata.cache.R")
library(data.table)
library(dplyr)

## airbox device
airbox.device <- get.data("device")

## airbox data
airbox.history <- get.data("history") %>%
  mutate(time = as.POSIXct(time)) %>%
  mutate(date = as.Date(time, "Asia/Taipei"))
