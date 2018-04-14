# ~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ESA Twitter metrics
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~
rm(list = ls())

library(plyr)
library(data.table)
library(ggplot2)
library(pander)

# Reading in all the analytics data
activityhistory <- read.csv("activityhistory.csv", header = T)
cumulativeexposure <- read.csv("cumulativeexposure.csv", header = T)
discovery <- read.csv("discovery.csv", header = T)
esa_tweets <- read.csv("esa_tweets.csv", header = T)
geoanalysis <- read.csv("geoanalysis.csv", header = T)
influential_stats <- read.csv("influential_stats.csv", header = T)
sentiment <- read.csv("sentiment.csv", header = T)
trendinglinks <- read.csv("trendinglinks.csv", header = T)
trendingphotos <- read.csv("trendingphotos.csv", header = T)
trendingphotos <- read.csv("trendingposts.csv", header = T)
trendingvideos <- read.csv("trendingvideos.csv", header = T)
