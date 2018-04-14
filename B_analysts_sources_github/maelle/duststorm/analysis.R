library("dplyr")
library("ggplot2")

load("data/weather.RData")
load("data/pm10.RData")



weather <- mutate(weather, dust_weather_code = (grepl("DU", presentwx)|
                                         grepl("DS",presentwx))) %>%  
  filter(as.Date(valid) >= lubridate::ymd("2016-05-15")) 

meas %>% 
  filter(city == "Delhi") %>% 
  filter(location != "Anand Vihar") %>%  
  filter(as.Date(dateLocal) >= lubridate::ymd("2016-05-15")) %>%
  ggplot() +
  theme_bw() +
  geom_segment(data = weather, aes(y = 0, yend = 1000,
                                   x = valid, xend = valid,
                                   col = dust_weather_code))+
  ylab(expression(paste("PM10 concentration (", mu, "g/",m^3,")"))) +
  xlab("Time") +
  ggtitle("PM10 & dust storm",
          subtitle = "PM10 data from the Indian government was accessed via OpenAQ.\n Codes related to dust were identified in METAR reports from the airport VIDP") +
  geom_point(aes(dateLocal, value)) +
  facet_grid(location ~.) +
  scale_colour_manual(values = c("white","yellow"))
ggsave("figures/plotDelhi.png", width = 8, height = 6)

