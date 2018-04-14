library("tidyverse")
library("ggmap")
library("gganimate")
library("animation")

read_file <- function(year){
  filename <- paste0("data/car_acc_", year, ".txt")
  if (year == 2015){
    read_delim(filename, delim = "\t", col_names = TRUE, 
               col_types = list(col_character(), col_character(), 
                                col_character(), col_character(), 
                                col_character(), col_character(), 
                                col_character(), col_character(), 
                                col_character(), col_character(), 
                                col_character())) %>%
      select(Id = NEZG_ID, Time = VREME_NEZ, lon = WGS_X, lat = WGS_Y, Type = VRSTA_NEZ)
  }else{
    read_delim(filename, delim = "\t", col_names = FALSE, 
               col_types = list(col_character(), col_character(), 
                                col_character(), col_character(), 
                                col_character(), col_character(), 
                                col_character())) %>%
      select(Id = X1, Time = X2, lon = X3, lat = X4, Type = X5)
  }
}

years <- 2015:2017

collisions <- years %>%
  purrr::map_df(read_file) %>%
  mutate(Id = as.integer(Id),
         Time = as.POSIXct(strptime(as.character(Time), format = "%d.%m.%Y,%R")),
         lon = as.double(str_replace(lon, ",", "\\.")),
         lat = as.double(str_replace(lat, ",", "\\."))) %>%
  filter(Time > "2015-01-01") %>%
  arrange(Time) %>%
  mutate(YW = strftime(Time, format = "%Y-%W"),
         M = strftime(Time, format = "%Y-%m"))

da_2 <- collisions %>%
  group_by(M) %>%
  summarise(Total = n()) %>%
  mutate(CumTotal = cumsum(Total),
         Time_f = paste0(strftime(strptime(paste0(M,"-01"), format = "%Y-%m-%d"), format = "%b %Y")),
         CumTotal_f = paste0("Total: ", format(CumTotal, big.mark = ",")))

# ggmap ----
bgd <- c(left = 20.35, bottom = 44.75, right = 20.55, top = 44.85)
map <- get_map(location = bgd, 
               zoom = 12, 
               color = "bw", 
               source = "stamen",
               maptype = "toner")

p <- ggmap(map, darken = 0.9) +
  geom_point(aes(x = lon, y = lat, frame = M, cumulative = TRUE), 
             colour = "#00BFFF", alpha = 0.1, pch = 16, size = 0.2, data = collisions) +
  geom_text(aes(x = 20.45, y = 44.845, hjust = "left"), label = "Traffic collisions in Belgrade | 2015-2017", size = 5, color = "white") +
  geom_text(aes(x = 20.36, y = 44.77, label = Time_f, frame = M, hjust = "left"), size = 5, color = "white", data = da_2) +
  geom_text(aes(x = 20.36, y = 44.76, label = CumTotal_f, frame = M, hjust = "left"), size = 5, color = "white", data = da_2) +
  labs(x = NULL, y = NULL) +
  theme(axis.text = element_blank(),
        axis.ticks = element_blank())

ani.options(ani.width = 600, ani.height = 430)
gganimate(p, "output.gif", interval = 0.3, title_frame = FALSE)