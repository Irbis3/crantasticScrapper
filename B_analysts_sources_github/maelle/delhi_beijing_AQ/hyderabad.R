library("ropenaq")
library("dplyr")
library("ggplot2")
library("viridis")
library("usaqmindia")
library("tidyr")
data("pm25_india")
############################################################
#                                                          #
#                   Get older India data                   ####
#                                                          #
############################################################

pm25_india <- filter(pm25_india, city == "Hyderabad")
pm25_india <- mutate(pm25_india, 
                     location = "US Diplomatic Post: New hyderabad")
pm25_india <- rename(pm25_india, value = conc)
pm25_india <- rename(pm25_india, dateLocal = datetime)

############################################################
#                                                          #
#          Get data from the two diplomatic posts          ####
#                                                          #
############################################################

count_india <- aq_measurements(city = "Hyderabad", parameter = "pm25",
                               location = "US+Diplomatic+Post%3A+Hyderabad",
                               date_to = as.character(Sys.Date() - 1))
count_india <- attr(count_india, "meta")$found
meas_india <- NULL
for(page in 1:ceiling(count_india/1000)){
  print(page)
  meas_india <- bind_rows(meas_india,
                          aq_measurements(city = "Hyderabad", parameter = "pm25",
                                          location = "US+Diplomatic+Post%3A+Hyderabad",
                                          limit = 1000, page = page))
}



count_china <- aq_measurements(city = "Beijing", parameter = "pm25",
                               location = "Beijing+US+Embassy")
count_china <- attr(count_china, "meta")$found
meas_china <- NULL
for(page in 1:ceiling(count_china/1000)){
  print(page)
  meas_china <- bind_rows(meas_china,
                          aq_measurements(city = "Beijing", parameter = "pm25",
                                          location = "Beijing+US+Embassy",
                                          limit = 1000, page = page))
}


############################################################
#                                                          #
#                  compare data for india                  ####
#                                                          #
############################################################
pm25_india <- mutate(pm25_india,
                     dateLocal = lubridate::force_tz(dateLocal, tzone = "UTC"))
common_dates <- pm25_india$dateLocal[pm25_india$dateLocal %in% meas_india$dateLocal]
for_check <- select(pm25_india, value, dateLocal)
for_check <- rename(for_check, embassy = value)
for_check <- filter(for_check, dateLocal %in% common_dates)
for_check <- filter(meas_india, dateLocal %in% common_dates) %>%
  select(value, dateLocal) %>%
  rename(openaq = value) %>%
  left_join(for_check, by = "dateLocal")

for_check_long <- gather(for_check, "where", "value", c(1, 3))

ggplot(for_check_long) +
  geom_point(aes(dateLocal, value, col = where)) +
  facet_grid(where ~ .) +
  scale_color_viridis(discrete = TRUE)

nrow(for_check)
sum(for_check$openaq != for_check$embassy, na.rm = TRUE)
non_neg <- filter(for_check, openaq > -999)
sum(non_neg$openaq != non_neg$embassy, na.rm = TRUE)

filter(for_check_long,
       lubridate::month(dateLocal) == 3 &
         lubridate::day(dateLocal) %in% c(15, 16, 17)) %>%
  ggplot() +
  geom_point(aes(dateLocal, value, col = where)) +
  facet_grid(where ~ .) +
  scale_color_viridis(discrete = TRUE)
############################################################
#                                                          #
#                   now bind everything                    ####
#                                                          #
############################################################
meas_india <- filter(meas_india,
                     !dateLocal %in% common_dates)
both <- meas_india
both <- select(both, dateLocal, city, value, location)
both <- bind_rows(both, pm25_india)

############################################################
#                                                          #
#       Filter what clearly looks like wrong values        ####
#                                                          #
############################################################


both <- filter(both, value > -1)
both <- filter(both, value < 1000)

# hyderabad only
hyderabad <- filter(both, city == "Hyderabad")
hyderabad %>% 
  group_by(day = as.Date(dateLocal)) %>%
  summarize(value = mean(value, na.rm = TRUE)) %>%
  filter(value < 900) %>%
  ggplot(aes(day, value)) +
  geom_point(size = 1.5, col = "darkred")+
  ylab(expression(paste("PM2.5 concentration (", mu, "g/",m^3,")"))) +
  ggtitle("Daily average PM2.5 concentration in Hyderabad, India",
          subtitle = "Data from the US consulate accessed via their website & OpenAQ.
The horizontal line is the WHO guideline of 25 µg/m3 24-hour mean.") +
  theme(text = element_text(size=16)) +
  geom_hline(yintercept = 25, size = 1.5) +
  xlab("Time (days)")
ggsave("hyderabad.png", width = 8.5, height = 6)



hyderabad %>% 
  filter(lubridate::year(dateLocal) == 2016 & lubridate::month(dateLocal) == 11) %>%
  group_by(day = as.Date(dateLocal)) %>%
  summarize(value = mean(value, na.rm = TRUE)) %>%
  ggplot(aes(day, value)) +
  geom_line(size = 1.5, col = "darkred")+
  ylab(expression(paste("PM2.5 concentration (", mu, "g/",m^3,")"))) +
  ggtitle("Daily average PM2.5 concentration in Hyderabad, India",
          subtitle = "Data from the US consulate accessed via their website & OpenAQ.
The horizontal line is the WHO guideline of 25 µg/m3 24-hour mean.") +
  theme(text = element_text(size=16)) +
  geom_hline(yintercept = 25, size = 1.5) +
  xlab("Time (days)")
ggsave("hyderabad_nov_2016.png", width = 8.5, height = 6)


hyderabad %>% 
  filter(lubridate::year(dateLocal) == 2016 & lubridate::month(dateLocal) == 11) %>%
  ggplot(aes(dateLocal, value)) +
  geom_line(size = 1.5, col = "darkred")+
  ylab(expression(paste("PM2.5 concentration (", mu, "g/",m^3,")"))) +
  ggtitle("Daily average PM2.5 concentration in Hyderabad, India",
          subtitle = "Data from the US consulate accessed via their website & OpenAQ.
          The horizontal line is the WHO guideline of 25 µg/m3 24-hour mean.") +
  theme(text = element_text(size=16)) +
  geom_hline(yintercept = 25, size = 1.5) +
  xlab("Time (hours)")
ggsave("hyderabad_hourly_nov_2016.png", width = 8.5, height = 6)