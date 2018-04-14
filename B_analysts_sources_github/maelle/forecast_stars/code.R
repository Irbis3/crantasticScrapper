library("gh")
library("lubridate")
library("ggplot2")
library("viridis")
library("dplyr")
library("tidyr")
library("viridis")
library("forecast")
library("janitor")

# get data, loop because pagination
stardates <- NULL

geht <- TRUE
page <- 1
while(geht){
  print(page)
  stars <- try(gh("/repos/robjhyndman/forecast/stargazers",
              .token = Sys.getenv("GITHUB_TOKEN"),
              .send_headers = c("Accept" = 'application/vnd.github.v3.star+json'),
              page = page))
  
  geht <- stars != ""
  
  if(geht){
    stardates <- c(stardates, vapply(stars, "[[", "", "starred_at"))
    page <- page + 1
    }
  
  
}

stardates <- lubridate::ymd_hms(stardates)

# make table of counts per day
star_table <- data.frame(time = stardates)
star_table <- mutate(star_table, date = as.Date(time))
star_table <- group_by(star_table, date) %>%
  summarize(n = n()) 
star_table <- mutate(star_table, 
                     cum_n = cumsum(n))

complete_dates <- seq(from = min(star_table$date),
                      to = max(star_table$date),
                      by = "1 day")

# prepare ts
ts_prep <- data.frame(date = complete_dates)
ts_prep <- left_join(ts_prep, star_table,
                     by = "date")
ts_prep <- mutate(ts_prep, cum_n = zoo::na.locf(cum_n))

ts_stars <- xts::xts(ts_prep$cum_n, ts_prep$date)
ts_stars = ts(ts_stars, freq=365, start=c(2012, 113))
autoplot(ts_stars)

# forecast
pred <- forecast(ets(ts_stars), h = 2000)

# plot
theme_set(theme_gray(base_size = 14))
autoplot(pred) +
  ylab("Number of forecast stargazers") +
  ggtitle("When will the forecast package reach 1,000 stargazers?",
          subtitle = "Data accessed from the Github API via Gabor Csardi's gh package")
ggsave(file = "forecast.png", width = 8, height = 6)
