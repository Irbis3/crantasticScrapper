locs <- geocode(as.character(unique(anom$name)), output = "more")
#save(locs, file = "locs.RData")
#load("locs.RData")

num_dis <- anom %>%
  filter(date >= "2014-08-01") %>%
  group_by(name) %>%
  summarise(count = sum(count))

num_dis <- left_join(num_dis, locs[,c("lon", "lat", "query")], 
                     by = c("name" = "query"))
num_dis[which(num_dis$name == "TAM, GONZALEZ"),]$lon <- -98.6667
num_dis[which(num_dis$name == "TAM, GONZALEZ"),]$lat <- 24.6167
num_dis[which(num_dis$name == "TAM, EL MANTE"),]$lon <- -99.1484
num_dis[which(num_dis$name == "TAM, EL MANTE"),]$lat <- 23.4683

mx <- readOGR("maps", "estatal")
bb <- bbox(as(extent(mx) , "SpatialPolygons"))
mx <- fortify(mx, region="CVEGEO")


ggplot(num_dis, aes(lon, lat), 
       color = "black") +
  geom_polygon(data = mx, aes(long, lat, group = group),
               size = .2, color = "black", fill = "transparent") +
  coord_map("albers", lat0 = bb[ 2 , 1 ] , lat1 = bb[ 2 , 2 ] )  + 
  geom_point(aes(size = count, fill = count), 
             shape = 21, color = "black", fill = "#084081") +
  #scale_fill_gradient2( 
  #  low = "#f7fcf0", high = "#084081", mid = "#7bccc4", space = "Lab",
  #  guide="none") +
  scale_size_area("number of disappeared\nfrom Sep 14 to Jan 15", max_size = 12) +
  ggtitle("MUNICIPALITIES WITH ANOMALIES IN DISAPPEARANCES\nFROM AUGUST 2014 TO JANUARY 2015") +
  #guides(fill = guide_legend(override.aes = list(size = 6)))+
  theme(legend.key = element_rect( fill = NA)) +
  infographic_theme2() +
  theme_bare()
ggsave("graphs/map.png", width = 9.6, height = 7.75, dpi = 100)