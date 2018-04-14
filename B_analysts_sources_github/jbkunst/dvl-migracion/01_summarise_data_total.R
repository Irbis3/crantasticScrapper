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
  
str_clean_country <- function(x) {
  x %>%
    str_replace("\\(.*\\)", "") %>%
    str_replace("\\[.*\\]", "") %>%
    str_trim() %>%
    str_to_lower() %>%
    str_replace("rep\\.", "republic") %>% # ominican republic
    str_to_title()
}

theme_set(thm)


# data --------------------------------------------------------------------
data <- read_csv("data/unhcr_popstats_export_time_series_all_data.csv", skip = 3)
glimpse(data)

data <- clean_names(data)
glimpse(data)

data <- rename(data, destination = country_territory_of_asylum_residence)

data <- data %>%
  mutate(destination = str_clean_country(destination),
         origin = str_clean_country(origin))

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

# latam -------------------------------------------------------------------
count(data_orig, origin)

filter(data_orig, str_detect(origin, "hai"))
filter(data_orig, str_detect(origin, "boli"))
filter(data_orig, str_detect(origin, "domi"))
filter(distinct(data_orig, origin), str_detect(origin, "saint"))

df_latam <- read_html("https://en.wikipedia.org/wiki/List_of_Latin_American_countries_by_population") %>%
  html_table(fill = TRUE) %>%
  first() %>%
  tbl_df() %>%
  clean_names()

df_latam

df_latam$country_or_dependent_territory

df_latam <- filter(df_latam, country_or_dependent_territory != "Total")

df_latam <- mutate(df_latam, country = str_clean_country(country_or_dependent_territory))

df_latam$country

# no encuentro 2
anti_join(df_latam, data_orig, by = c( "country" = "origin")) %>%
  count(country)

data_orig <- semi_join(data_orig, df_latam, by = c("origin" = "country"))
# count(data_orig_latam, origin)

data_dest <- semi_join(data_dest, df_latam, by = c("destination" = "country"))
# count(data_dest_latam, destination)


# exploring ---------------------------------------------------------------
data_orig %>% 
  filter(year > 1990) %>% 
  filter(!origin %in% c("Martinique", "Guadeloupe")) %>% 
  ggplot() + 
  geom_line(aes(year, value)) + 
  scale_y_comma() + 
  expand_limits(y = 0) + 
  facet_wrap(~ origin, scales = "free_y") + 
  labs(title = "Origin considering ALL destination")

ggsave("plot/latam_origin_total.png", width = 16, height = 9)


data_dest %>% 
  filter(year > 1990) %>% 
  filter(!destination %in% c("Martinique", "Guadeloupe")) %>% 
  ggplot() + 
  geom_line(aes(year, value)) + 
  scale_y_comma() + 
  facet_wrap(~ destination, scales = "free_y") + 
  labs(title = "Destinations considering ALL origins")

ggsave("plot/latam_destination_total.png", width = 16, height = 9)


data_orig %>% 
  filter(origin == "Haiti") %>% 
  filter(year > 1990) %>% 
  filter(!origin %in% c("Martinique", "Guadeloupe")) %>% 
  ggplot() + 
  geom_line(aes(year, value)) + 
  scale_y_comma() + 
  expand_limits(y = 0) + 
  facet_wrap(~ origin, scales = "free_y") + 
  labs(title = "Origin considering ALL destination")

ggsave("plot/haiti_origin_total.png", width = 16, height = 9)


gc()



