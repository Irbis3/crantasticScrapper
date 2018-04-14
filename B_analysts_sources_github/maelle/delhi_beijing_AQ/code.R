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

pm25_india <- filter(pm25_india, city == "Delhi")
pm25_india <- mutate(pm25_india, 
                     location = "US Diplomatic Post: New Delhi")
pm25_india <- rename(pm25_india, value = conc)
pm25_india <- rename(pm25_india, dateLocal = datetime)

############################################################
#                                                          #
#          Get data from the two diplomatic posts          ####
#                                                          #
############################################################

count_india <- aq_measurements(city = "Delhi", parameter = "pm25",
                               location = "US+Diplomatic+Post%3A+New+Delhi",
                               date_to = as.character(Sys.Date() - 1))
count_india <- attr(count_india, "meta")$found
meas_india <- NULL
for(page in 1:ceiling(count_india/1000)){
  print(page)
  meas_india <- bind_rows(meas_india,
                          aq_measurements(city = "Delhi", parameter = "pm25",
                                          location = "US+Diplomatic+Post%3A+New+Delhi",
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
both <- bind_rows(meas_china, meas_india)
both <- select(both, dateLocal, city, value, location)
both <- bind_rows(both, pm25_india)

############################################################
#                                                          #
#       Filter what clearly looks like wrong values        ####
#                                                          #
############################################################


both <- filter(both, value > -1)
both <- filter(both, value < 1985)

# delhi only
delhi <- filter(both, city == "Delhi")
delhi %>% 
  group_by(day = as.Date(dateLocal)) %>%
  summarize(value = mean(value, na.rm = TRUE)) %>%
  ggplot(aes(day, value)) +
  geom_point(size = 1.5, col = "darkred")+
  ylab(expression(paste("PM2.5 concentration (", mu, "g/",m^3,")"))) +
  ggtitle("Daily average PM2.5 concentration in Delhi, India",
          subtitle = "Data from the US embassy accessed via their website & OpenAQ.
The horizontal line is the WHO guideline of 25 µg/m3 24-hour mean.") +
  theme(text = element_text(size=16)) +
  geom_hline(yintercept = 25, size = 1.5) +
  xlab("Time (days)")
ggsave("Delhi.png", width = 8.5, height = 6)



delhi %>% 
  filter(lubridate::year(dateLocal) == 2016 & lubridate::month(dateLocal) == 11) %>%
  group_by(day = as.Date(dateLocal)) %>%
  summarize(value = mean(value, na.rm = TRUE)) %>%
  ggplot(aes(day, value)) +
  geom_line(size = 1.5, col = "darkred")+
  ylab(expression(paste("PM2.5 concentration (", mu, "g/",m^3,")"))) +
  ggtitle("Daily average PM2.5 concentration in Delhi, India",
          subtitle = "Data from the US embassy accessed via their website & OpenAQ.
The horizontal line is the WHO guideline of 25 µg/m3 24-hour mean.") +
  theme(text = element_text(size=16)) +
  geom_hline(yintercept = 25, size = 1.5) +
  xlab("Time (days)")
ggsave("Delhi_nov_2016.png", width = 8.5, height = 6)
############################################################
#                                                          #
#                      last year only                      ####
#                                                          #
############################################################


both <- filter(both, as.Date(dateLocal) >= lubridate::ymd("2015 11 5"))

############################################################
#                                                          #
#                       missingness                        ####
#                                                          #
############################################################
both %>%
  mutate(week = as.factor(paste(lubridate::year(dateLocal),
                                stringr::str_pad(lubridate::week(dateLocal),
                                                 width = 2, pad = "0")))) %>%
  group_by(week, location) %>%
  summarize(missingness = 1 - n()/168,
            quantile10 = quantile(value, p = 0.1),
            quantile90 = quantile(value, p = 0.9),
            value = mean(value)) %>%
  readr::write_csv("weekly_summary.csv")
    

############################################################
#                                                          #
#                          figure                          ####
#                                                          #
############################################################


both %>%
  mutate(week = as.factor(paste(lubridate::year(dateLocal),
                                stringr::str_pad(lubridate::week(dateLocal),
                                                 width = 2, pad = "0")))) %>%
  group_by(week, location) %>%
  summarize(missingness = 1 - n()/168,
    quantile10 = quantile(value, p = 0.1),
    quantile90 = quantile(value, p = 0.9),
    value = mean(value)) %>%
  mutate(how_complete = 4 * (1-missingness)) %>%
ggplot() +
  geom_point(aes(week, value, col = location), size = 2) +
  geom_segment(aes(x = week, xend = week,
                   y = quantile10, yend = quantile90,
                   col = location, size = how_complete), alpha = 0.5) +
  scale_color_viridis(discrete = TRUE, end = 0.8) +
  theme(legend.position = "bottom")+
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        text = element_text(size=20)) +
  ylab(expression(paste("PM2.5 concentration (", mu, "g/",m^3,")"))) +
  ggtitle("Air quality in Beijing, China and Delhi, India in the last year",
          subtitle = "Data from the US embassies accessed via their websites & OpenAQ via ropenaq.
The point is the weekly mean, the segment goes from the 10th to the 90th quantile of all values from the week.
The width of the segment indicates how complete the week is, i.e. how many of the expected 168 values there are.")

ggsave("Beijing_Delhi.png", width = 16, height = 6)