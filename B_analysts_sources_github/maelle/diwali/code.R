library("ggmap")
library("ggplot2")
library("ropenaq")
library("dplyr")
library("tidyr")

measurementsIndia <- NULL
for (page in 1:6){
  print(page)
  measurementsIndia <- rbind(measurementsIndia,
                             aq_measurements(country = "IN", 
                                             has_geo = TRUE, 
                                             page = page,
                                             parameter = "pm25", 
                                             limit = 1000,
                                             date_from = "2016-10-28",
                                             value_from = 0))}

measurementsIndia <- measurementsIndia %>%
  group_by(hour = update(dateLocal, minute = 0),
           location, longitude, latitude) %>%
  summarize(value = mean(value))

measurementsIndia <- complete(measurementsIndia, hour, location, longitude,
                              latitude, fill = list(value = NA))

library("gganimate")
library("animation")
indiaMap <- get_map(location = c(65,
                                 6,
                                 97,
                                 36))

minConc <- min(measurementsIndia$value, na.rm = TRUE)
maxConc <- max(measurementsIndia$value, na.rm = TRUE)
plotMap <- ggmap(indiaMap)+ theme_bw()+
  geom_point(data = measurementsIndia, aes(x=longitude,
                                           y=latitude),
             col = "grey50", size = 8)+
  geom_point(data = measurementsIndia, aes(x=longitude,
                                    y=latitude,
                                    frame=hour,
                                    colour = value),
             size=8)+
  viridis::scale_color_viridis(trans = "log") +
  theme(axis.line=element_blank(),axis.text.x=element_blank(),
        axis.text.y=element_blank(),axis.ticks=element_blank(),
        axis.title.x=element_blank(),
        text = element_text(size=20),
        axis.title.y=element_blank(),
        panel.background=element_blank(),panel.border=element_blank(),panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),plot.background=element_blank())+
  ggtitle("PM 2.5 concentration") +
  theme(plot.title = element_text(lineheight=1, face="bold"))

ani.options(interval = 0.25, ani.width = 800, ani.height = 800)
gg_animate(plotMap, "map.mp4")