library("ggmap")
library("ggplot2")
library("ropenaq")
library("dplyr")
library("tidyr")
library("gganimate")
library("animation")

count <- aq_measurements(country = "US", 
                         has_geo = TRUE, 
                         parameter = "pm25", 
                         date_from = "2016-12-31",
                         date_to = "2017-01-02",
                         value_from = 0)
count <- ceiling(attr(count, "meta")$found/1000)
measurements <- NULL
for (page in 1:count){
  print(page)
  measurements <- rbind(measurements,
                        aq_measurements(country = "US", 
                                        has_geo = TRUE, 
                                        page = page,
                                        parameter = "pm25", 
                                        limit = 1000, 
                                        date_from = "2016-12-31",
                                        date_to = "2017-01-02",
                                        value_from = 0))}

measurements <- measurements %>% 
  group_by(hour = update(dateUTC, minute = 0),
           location, longitude, latitude, dateUTC) %>%
  summarize(value = mean(value))
measurements <- group_by(measurements, location) %>%
  filter(all(!is.na(value))) %>%
  ungroup()

measurements <- measurements %>%
  ungroup() %>%
  mutate(hour = update(hour, hour = lubridate::hour(hour) - 5)) %>%
  mutate(value = ifelse(value > 80, 80, value))
save(measurements, file = "data/nye.RData")




mintime <- lubridate::ymd_hms("2016 12 31 17 00 00")
maxtime <- lubridate::ymd_hms("2017 01 01 07 00 00")

measurements <- filter(measurements, 
                       hour >= mintime)
measurements <- filter(measurements, 
                       hour <= maxtime)


usmap <- get_map(location = c(min(measurements$longitude),
                              min(measurements$latitude),
                              max(measurements$longitude),
                              max(measurements$latitude)))

lala <- group_by(measurements, location, latitude) %>% summarize(n = n())

measurements <- group_by(measurements, location, latitude) %>%
  filter(n() == max(lala$n)) %>%
  ungroup()

minConc <- min(measurements$value, na.rm = TRUE)
maxConc <- max(measurements$value, na.rm = TRUE)
plotMap <- ggmap(usmap)+ theme_bw()+
  geom_point(data = measurements, aes(x=longitude,
                                      y=latitude,
                                      frame=as.character(hour),
                                      colour = value),
             size=2)+
  viridis::scale_color_viridis(expression(atop(paste("PM2.5 concentration (", mu, "g/",m^3,")"),"Set to 80 if >80")),
                               option = "inferno") +
  theme(axis.line=element_blank(),axis.text.x=element_blank(),
        axis.text.y=element_blank(),axis.ticks=element_blank(),
        axis.title.x=element_blank(),
        text = element_text(size=16),
        axis.title.y=element_blank(),
        panel.background=element_blank(),panel.border=element_blank(),panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),plot.background=element_blank())+
  ggtitle("PM2.5 concentration",
          subtitle = "New York local time. Data accessed from OpenAQ via ropenaq.") +
  theme(plot.title = element_text(lineheight=1, face="bold"))

ani.options(interval = 0.5, ani.width = 800, ani.height = 800)
file.remove("videos/map_nye.gif")
gg_animate(plotMap, "videos/map_nye.gif")