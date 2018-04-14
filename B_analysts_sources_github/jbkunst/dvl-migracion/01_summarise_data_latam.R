# packages and helpers ----------------------------------------------------
rm(list = ls())
library(tidyverse) # dplyr y ggplot2
library(gsheet)
library(janitor)   # clean names
library(rvest)     # downloa latam countries 
library(stringr)   #
library(hrbrthemes)

thm <- 
  theme_ipsum(
    base_family = "Calibri Light",
    plot_margin = margin(10, 10, 10, 10)
  ) +
  theme(
    panel.grid.major = element_line(colour = "grey90"),
    panel.grid.minor = element_line(colour = "grey90"),
    legend.position = "bottom"
  )
  
theme_set(thm)


# data --------------------------------------------------------------------
data <- gsheet2tbl("https://docs.google.com/spreadsheets/d/16LaaLlYg4LfyzrLVj__kZYh-sxrb1H195KbulkqELlU/edit#gid=1742414770")
glimpse(data)

data <- rename(data, destination = destiny)

# destination / origin ----------------------------------------------------
data_orig <- data %>% 
  group_by(year, origin) %>% 
  summarise(value = sum(value)) %>% 
  ungroup()
data_orig

data_dest <- data %>% 
  group_by(year, destination) %>% 
  summarise(value = sum(value)) %>% 
  ungroup()
data_dest

# exploring ---------------------------------------------------------------
data_orig %>% 
  filter(year > 1990) %>% 
  filter(!origin %in% c("Martinique", "Guadeloupe")) %>% 
  ggplot() + 
  geom_line(aes(year, value)) + 
  scale_y_comma() + 
  expand_limits(y = 0) + 
  facet_wrap(~ origin, scales = "free_y") + 
  labs(title = "Origin considering LATAM destinations")

ggsave("plot/latam_origin.png", width = 16, height = 9)


data_dest %>% 
  filter(year > 1990) %>% 
  filter(!destination %in% c("Martinique", "Guadeloupe")) %>% 
  ggplot() + 
  geom_line(aes(year, value)) + 
  scale_y_comma() + 
  facet_wrap(~ destination, scales = "free_y") + 
  labs(title = "Destinations considering LATAM origins")

ggsave("plot/latam_destination.png", width = 16, height = 9)


data_orig %>% 
  filter(origin == "Haiti") %>% 
  filter(year > 1990) %>% 
  filter(!origin %in% c("Martinique", "Guadeloupe")) %>% 
  ggplot() + 
  geom_line(aes(year, value)) + 
  scale_y_comma() + 
  expand_limits(y = 0) + 
  facet_wrap(~ origin, scales = "free_y") + 
  labs(title = "Origin considering LATAM destination")

ggsave("plot/haiti_origin.png", width = 16, height = 9)

gc()








