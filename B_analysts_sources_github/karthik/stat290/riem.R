library(tidyverse)
library(readxl)
library(riem)
library(xkcd)
library(extrafont)
library(ggrepel)

# Download the file here:
# https://www.bts.gov/content/passengers-boarded-top-50-us-airports
us_airports <-
  readxl::read_excel(
    "~/Downloads/table_01_44-q417.xlsx",
    range = "A5:B54",
    col_names = c("Name", "Code")
  )


summer_weather <-
  purrr::map_df(us_airports$Code,
                riem::riem_measures,
                date_start = "2017-06-01",
                date_end = "2017-08-31")
# write_csv(summer_weather, path = "summer_weather.csv")
# summer_weather <- read_csv("summer_weather.csv")

winter_weather <-
  purrr::map_df(us_airports$Code,
                riem::riem_measures,
                date_start = "2016-12-01",
                date_end = "2017-02-28")

# write_csv(winter_weather, path = "winter_weather.csv")
# winter_weather <- read_csv("winter_weather.csv")


summer_weather <-
  dplyr::filter(summer_weather, !is.na(tmpf), !is.na(dwpf))
winter_weather <- dplyr::filter(winter_weather, !is.na(tmpf))

summer_weather <- dplyr::mutate(
  summer_weather,
  tmpc = weathermetrics::convert_temperature(tmpf,
         old_metric = "f", new_metric = "c"),
  dwpc = weathermetrics::convert_temperature(dwpf,
         old_metric = "f", new_metric = "c")
)

winter_weather <- dplyr::mutate(winter_weather,
                  tmpc = weathermetrics::convert_temperature(tmpf,
                  old_metric = "f", new_metric = "c"))


calculate_humidex <- function(temp, dewpoint) {
  temp + 0.5555 * (6.11 * exp(5417.7530 * (1 / 273.16 - 1 / (273.15 + dewpoint))) - 10)
}

# Maëlle says: I quickly checked that there was “nearly no missing data” so I didn’t remove
# any station nor day but if I were doing this analysis for something serious
# I’d do more checks including the time difference between measures for
# instance. Note that I end up with 48 airports only, there was no measure for
# Honolulu, HI (Honolulu International), San Juan, PR (Luis Munoz Marin
# International). Too bad!

# Humidex = Humidity index https://en.wikipedia.org/wiki/Humidex

# Range of humidex: Scale of comfort:[2][3]
#
# 20 to 29: Little to no discomfort
# 30 to 39: Some discomfort
# 40 to 45: Great discomfort; avoid exertion
# Above 45: Dangerous; heat stroke quite possible

summer_weather <- dplyr::mutate(summer_weather,
                  humidex = calculate_humidex(tmpc, dwpc))
summer_values <- summer_weather %>%
  dplyr::group_by(station) %>%
  dplyr::summarise(summer_humidex = mean(humidex, na.rm = TRUE))




winter_values <- winter_weather %>%
  dplyr::group_by(station) %>%
  dplyr::summarise(winter_tmpc = mean(tmpc, na.rm = TRUE))



climates <- dplyr::left_join(winter_values, summer_values,
                             by = "station")

climates <- dplyr::left_join(climates, us_airports,
                             by = c("station" = "Code"))

# This is so we have clean city names instead of just airports

climates <- dplyr::mutate(climates,
                          city = stringr::str_replace(Name, " \\(.*", ""))


# For the rest of this to work, you will need to install the xkcd font

xrange <- range(climates$summer_humidex)
yrange <- range(climates$winter_tmpc)

set.seed(42)

ggplot(climates,
       aes(summer_humidex, winter_tmpc)) +
  geom_point() +
  geom_text_repel(aes(label = city),
                  family = "xkcd",
                  max.iter = 50000) +
  ggtitle("Where to live based on your temperature preferences",
          subtitle = "Data from airports weather stations, 2016-2017") +
  xlab("Summer heat and humidity via Humidex") +
  ylab("Winter temperature in Celsius degrees") +
  xkcdaxis(xrange = xrange,
           yrange = yrange) +
  theme_xkcd() +
  theme(text = element_text(size = 16, family = "xkcd"))