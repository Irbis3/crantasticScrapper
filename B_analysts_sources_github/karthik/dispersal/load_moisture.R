# This script loads all moisture data for the 6 different sampling periods for year1.
rm(list=ls())
library(stringr)
setwd('~/Github/postdoc/dispersal_analysis')
week_1 <- read.csv(file='data/week1_dispersal_moisture.csv')
week_1$week <- "w1"
names(week_1) <- c("plot", "direction", "moisture","week")
week_2 <- read.csv(file='data/week2_dispersal_moisture.csv')
week_2$week <- "w2"
names(week_2) <- c("plot", "direction", "moisture","week")
week_3 <- read.csv(file='data/week3_dispersal_moisture.csv')
week_3$week <- "w3"
names(week_3) <- c("plot", "direction", "moisture","week")
week_4 <- read.csv(file='data/week4_dispersal_moisture.csv')
week_4$week <- "w4"
names(week_4) <- c("plot", "direction", "moisture","week")
week_5 <- read.csv(file='data/week5_dispersal_moisture.csv')
week_5$week <- "w5"
names(week_5) <- c("plot", "direction", "moisture","week")
week_6 <- read.csv(file='data/week6_dispersal_moisture.csv')
week_6$week <- "w6"
names(week_6) <- c("plot", "direction", "moisture","week")

moisture_data <- rbind(week_1, week_2, week_3, week_4, week_5, week_6)

save(moisture_data, file="data/moisture_data.rda")
