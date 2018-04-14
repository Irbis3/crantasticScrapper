library(ggplot2)
library(plyr)
library(stringr)
library(maps)
library(lubridate)

## @knitr function
convert_time <- function(time) {
as.POSIXlt(time, origin="1970-01-01", tz="UTC")
}


## @knitr cum_exposure
cum_exposure <- read.csv('data/cumulativeexposure.csv', header = T)
cum_exposure$time <- convert_time(cum_exposure$timestamp)
cum_exposure <- cum_exposure[, -c(1:2)]


## @knitr cumplot
cumulative_plot <- ggplot(cum_exposure, aes(time, cumulative_exposure)) + geom_line(size=1, colour="#18699c") + geom_point(colour="gray") + ylab("Cumulative exposure") + xlab("Date") + ggtitle("Cumulative exposure of ESA tweets")
print(cumulative_plot)



## @knitr activity_history
ahistory <- read.csv('data/activityhistory.csv', header = T)
ahistory <- ahistory[, -c(1:2)]
ahistory$time <- convert_time(ahistory$timestamp)

## @knitr aplot
hist_plot <- ggplot(ahistory, aes(time, mentions)) + geom_line() + geom_point(aes(size=1.1+influential)) + ggtitle("Mentions of ESA 2012 over time")
print(hist_plot)

## @knitr discovery
discovery <- read.csv('data/discovery.csv', header = T)
discovery_data <- discovery[,1:33]
discovery_table <- discovery[,c(1,2,34:51)]
d_data <- melt(discovery_data, id.vars=1:2)
d_data$variable <- str_sub(d_data$variable, 2)
d_data$variable <- mdy(d_data$variable)
d_data$value <- as.numeric(d_data$value)

## @knitr dplot
discovery_plot <- ggplot(subset(d_data, related.term!=""), aes(variable, value)) + geom_line() + facet_wrap(~ related.term, scales="free_y") + opts(axis.text.x = theme_text(angle =-90))
print(discovery_plot)

## @knitr geoanalysis
geodata <- read.csv('data/geoanalysis.csv', header = T)
geodata <- subset(geodata, Mentions > 0)
geodata <- subset(geodata, Region !="World")
geodata <- subset(geodata, Region !="Virgin Islands")

## @knitr geoplot
geoplot <- ggplot(subset(geodata, Query=="#esa2012"), aes(Region, Mentions)) + geom_point() + opts(axis.text.x = theme_text(angle =-90))
print(geoplot)

## @knitr sentiment
sentiment <- read.csv('data/sentiment.csv', header = T)
sentiment <- sentiment[, -c(1:2)]
sentiment$date <- convert_time(sentiment$timestamp)

## @knitr splot
ggplot(sentiment, aes(date)) + geom_line(aes(y=positive_sentiment), colour="#18699c",size=1.5) +  geom_line(aes(y=negative_sentiment), colour="#a32e3a",size=1.5) + ylab("Count") + xlab("Time")  + ggtitle("Sentiment of tweets under #ESA2012 hashtag (blue = positive, red = negative)")

## @knitr totalplot
ggplot(sentiment, aes(date)) + ggtitle("Number of tweets under #ESA2012 hashtag") +   geom_line(aes(y=mentions), colour="#474747",size=1.5) + ylab("Count") + xlab("Time")

## @knitr rstats
rstats <- subset(d_data, related.term=="#rstats")
rstats_plot <- ggplot(rstats[15:20,], aes(variable, value)) + geom_line(size=1.5)  + opts(axis.text.x = theme_text(angle =-90))
print(rstats_plot)


## @knitr esasocial
esasocial <- subset(d_data, related.term=="#esasocial")
esasocial_plot <- ggplot(esasocial[15:20,], aes(variable, value)) + geom_line(size=1.5)  + opts(axis.text.x = theme_text(angle =-90))
print(esasocial_plot)

