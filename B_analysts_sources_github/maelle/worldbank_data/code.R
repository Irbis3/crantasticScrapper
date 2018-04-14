library("dplyr")
library("tidyr")
library("countrycode")
library("rworldmap")
library("ggplot2")
library("viridis")
library("gganimate")
library("animation")
# world map
map.world <- map_data(map="world")
map.world <- mutate(map.world,
                    region = countrycode(region, "country.name", "iso2c"))

# world bank data
data <- readxl::read_excel("data/API_IP.JRN.ARTC.SC_DS2_en_excel_v2.xls",
                           skip = 3)
names(data) <- gsub(" ", "", names(data))
data <- mutate(data, CountryCode = countrycode(CountryCode, "iso3c", "iso2c"))
data <- filter(data, !is.na(CountryCode))

data <- gather(data, "year", "number_articles", 5:ncol(data))

map.world <- filter(map.world,
                     region  %in% unique(data$CountryCode))

data <- group_by(data, year) %>%
  mutate(sum = sum(number_articles, na.rm = TRUE)) %>%
  filter(sum != 0) %>%
  ungroup()

map.world <- left_join(map.world, data, by = c("region" = "CountryCode"))

# viz 
gg <- ggplot()
gg <- gg + geom_map(data = map.world, map = map.world,
                    aes(x = long, y = lat, map_id = region,
                        fill = log(number_articles),
                        frame = year),
                    color = "#2b2b2b", size = 0.1)
gg <- gg + scale_fill_viridis()
gg <- gg + ggthemes::theme_map() +
  theme(text = element_text(size=20))
gg <- gg + labs(title = "Number of scientific and technical journal articles per country",
       subtitle = "World bank data. Scientific and technical journal articles refer to the number of 
scientific and engineering articles published in the following fields: physics, biology, 
chemistry, mathematics, clinical medicine, biomedical research, engineering
and technology, and earth and space sciences.")
ani.options(interval = 1, ani.width = 800, ani.height = 800)
gg_animate(gg, "output2.mp4")