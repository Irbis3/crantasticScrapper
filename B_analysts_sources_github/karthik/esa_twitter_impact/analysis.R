# First pass at tweet analysis
library(ggplot2)
library(plyr)
library(stringr)

tweets <- read.csv(file = "~/Dropbox/esa_twitter_impact/data/cleaned_twitter_data.csv", header = TRUE)
date_breakdown <- dcast(tweets, tweet_date ~., length)
names(date_breakdown)[2] <- "count"
ggplot(date_breakdown, aes(tweet_date, count)) + geom_point(size=1.2)

