library("ggplot2")
library("ropenaq")
library("dplyr")
library("ggmap")
library("ggrepel")
library("lubridate")
load("data/PM25.RData")
map_india <- get_map(location = "Delhi")
save(map_india, file = "data/map_india.RData")

loc_india <- ropenaq::aq_locations(city = "Delhi", 
                                   parameter = "pm10")$results %>% 
  filter(location != "Anand Vihar")

load("data/india_airports.RData")
india_airports <- filter(india_airports, id == "VIDP")
india_airports <- rename(india_airports, 
                         longitude = lon,
                         latitude = lat) %>%
  mutate(place = "Airport",
         name = "Indira Gandhi Airport")
loc_india <- mutate(loc_india, place = "PM10 monitor") %>%
  rename(name = location) %>%
  filter(name != "RK Puram")
data_geo <- bind_rows(loc_india, india_airports)

gg <- ggmap(map_india) +
  geom_point(data = data_geo,
             aes(longitude, latitude,
                 col = place)) +
  theme(axis.line=element_blank(),axis.text.x=element_blank(),
        axis.text.y=element_blank(),axis.ticks=element_blank(),
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        panel.background=element_blank(),panel.border=element_blank(),panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),plot.background=element_blank())+
  ggtitle("Data sources in Delhi, PM10 & weather") +
  theme(plot.title = element_text(lineheight=1, face="bold")) +
  geom_label_repel(data = data_geo,
            aes(longitude, latitude,
                label = name), max.iter = 10000)
ggsave(file = "figures/map.png", width = 6, height = 6)