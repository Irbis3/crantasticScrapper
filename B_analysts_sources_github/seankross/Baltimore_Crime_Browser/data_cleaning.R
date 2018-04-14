# Downloaded from https://data.baltimorecity.gov/Public-Safety/BPD-Part-1-Victim-Based-Crime-Data/wsfq-mvij
# 2016-09-27 20:18:20 EDT
# File downloaded as crime.csv

library(dplyr)
library(lubridate)
library(readr)
library(magrittr)
library(stringr)
library(purrr)

crime <- read_csv("crime.csv")

colnames(crime)[which(colnames(crime) == "Inside/Outside")] <- "Inside_Outside"
colnames(crime)[which(colnames(crime) == "Location 1")] <- "Location_1"

crime %<>%
  mutate(CrimeDate = mdy(CrimeDate)) %>%
  mutate(CrimeTime = hms(CrimeTime)) %>%
  mutate(Inside_Outside = recode(Inside_Outside, "I" = "Inside", 
                                 "O" = "Outside"))

crime$lat <- crime$Location_1 %>%
  map_chr(function(x){str_extract_all(x, "-?[0-9]{2}\\.[0-9]+")[[1]][1]})
crime$lng <- crime$Location_1 %>%
  map_chr(function(x){str_extract_all(x, "-?[0-9]{2}\\.[0-9]+")[[1]][2]})

crime %<>%
  select(-Post, -Location_1, -`Total Incidents`) %>%
  filter(!is.na(lat)) %>%
  filter(!is.na(lng))

crime %<>%
  filter(!is.na(CrimeDate)) %>%
  mutate(popdate = paste("Date:", CrimeDate)) %>%
  mutate(content = paste(popdate, Location, Description, sep = "<br/>"))

saveRDS(crime, "crime.rds")
