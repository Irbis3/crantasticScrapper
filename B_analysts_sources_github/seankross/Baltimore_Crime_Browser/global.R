crime <- readRDS("crime.rds")
crime$lat <- as.numeric(crime$lat)
crime$lng <- as.numeric(crime$lng)