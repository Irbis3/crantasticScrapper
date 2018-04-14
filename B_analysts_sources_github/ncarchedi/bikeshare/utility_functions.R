library(assertthat)
library(data.table)
library(dplyr)
library(ggplot2)
library(lubridate)
library(tidyr)

read_and_prep_data <- function(filename) {
  dat <- tbl_df(fread(filename))
  dat$datetime <- ymd_hms(dat$datetime)
  dat$season <- factor(dat$season)
  dat$holiday <- factor(dat$holiday)
  dat$workingday <- factor(dat$workingday)
  dat$weather <- factor(dat$weather)
  dat
}

add_vars <- function(df) {
  df %>%
    mutate(month = as.factor(month(datetime)), 
           day = as.factor(day(datetime)), 
           hour = as.factor(hour(datetime))) %>%
    group_by(month) %>% 
    mutate(prev1 = lag(count, 1), prev2 = lag(count, 2)) %>%
    ungroup %>%
    na.omit
}

compute_rmsle <- function(p, a) {
  assert_that(length(p) == length(a))
  assert_that(all(p >= 0), all(a >= 0))

  lp <- log(p + 1)
  la <- log(a + 1)

  sqrt(mean((lp - la) ^ 2))
}
