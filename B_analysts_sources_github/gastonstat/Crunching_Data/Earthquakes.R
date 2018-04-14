# =========================================================================
# Title:        Earthquakes.R
# Author:       Gaston Sanchez
# Date:         May, 2012
# Description:  Code in R to visualize earthquakes in Northern California
#
# License:      BSD Simplified License
#               http://www.opensource.org/license/BSD-3-Clause
#               Copyright (c) 2012, Gaston Sanchez
#               All rights reserved
# =========================================================================

# Visualizing Earthquakes in Northern California 
# Ther's tons of data in http://earthquake.usgs.gov/research/data/
# For this example I downloaded data from the catalog of Northern CA: 
# http://quake.geo.berkeley.edu/ncedc/catalog-search.html
# Search catalog Northern CA
# look for earthquakes from 2008 - 2011
# get data in CSV format


# packages
library(ggplot2)
library(maps)
library(ggmap)
library(mapproj)


# set working directory (you need to specify your won working directory!)
setwd("/Users/Earthquakes")

# import data
quakes = read.csv("earthquakes.csv", stringsAsFactors=FALSE)


## Process first column 'DateTime'
# split DateTime
date.time = strsplit(quakes$DateTime, " ")

# get dates
quake.date = sapply(date.time, function(x) x[1])

# add date (converted as.Date)
quakes$Date = as.Date(quake.date)

# add year
quakes$Year = b = as.numeric(substr(quake.date, 1, 4))

# get times
quake.time = sapply(date.time, function(x) x[2])

# add hour
quakes$Hour = as.numeric(substr(quake.time, 1, 2))

# add minutes
quakes$Mins = as.numeric(substr(quake.time, 4, 5))

# add seconds
quakes$Secs = as.numeric(substr(quake.time, 7, 8))

# stupid plot
with(quakes, plot(Longitude, Latitude, pch=20, col="gray70"))



## Let's do simple plot with library maps
# map of State of California with counties
map("county", "california")
points(quakes$Longitude, quakes$Latitude, col="red")
title("Earthquakes in Northern California 2008 - 2011", cex.main=1)

# map of california
map("county", "california", fill=TRUE, col="gray80")
points(quakes$Longitude, quakes$Latitude, pch=19, col=hsv(0.65, 1, 1, 0.5))
title("Earthquakes in Northern California 2008 - 2011",
	cex.main=1, col.main="gray60")
	
# lets make a nicer plot
map("county", "california", fill=TRUE, col="gray30", bg="gray20", lwd=0.5)
points(quakes$Longitude, quakes$Latitude, pch=19, col=hsv(0.15, 1, 0.8, 0.3),
	cex= 2 * quakes$Magnitude / max(quakes$Magnitude))
title("Earthquakes in Northern California 2008 - 2011",
	cex.main=1, col.main="gray70")


## Another option is to use ggplot
# map of california with counties
ca_map = map_data("county", "california")

# simple map with ggplot
ggplot(data=quakes, aes(Longitude, Latitude)) + 
geom_polygon(data=ca_map, aes(long, lat, group=group)) + 
geom_point(aes(colour=Magnitude, size=Magnitude)) + 
opts(title = "Earthquakes in Northern California 2008 - 2011")

# let's try to get a nicer plot
ggplot(data=quakes, aes(Longitude, Latitude)) + 
geom_polygon(data=ca_map, aes(long, lat, group=group), colour="gray10", size=0.3) + 
geom_point(aes(colour=Magnitude, size=Magnitude), alpha=0.7) + 
labs(x="", y="") + 
scale_colour_gradient(low="tomato", high="maroon") + 
opts(title = "Earthquakes in Northern California 2008 - 2011",
	panel.background = theme_rect(fill="gray10", colour="gray35"),
	axis.text.x = theme_blank(),
	axis.text.y = theme_blank(),
	axis.ticks = theme_blank(),
	panel.grid.major = theme_blank(),
	panel.grid.minor = theme_blank(),
	plot.title = theme_text(colour="gray30", size=13))


# let's make a more interesting plot
ggplot(data=quakes, aes(Longitude, Latitude)) + 
geom_polygon(data=ca_map, aes(long, lat, group=group), colour="gray10", size=0.3) + 
geom_point(aes(colour=Magnitude, size=Magnitude), alpha=0.7) + 
labs(x="", y="") + 
scale_colour_gradient(low="tomato", high="maroon") + 
facet_wrap(~ Year) + 
opts(title = "Earthquakes in Northern California by Year",
	panel.background = theme_rect(fill="gray10", colour="gray35"),
	axis.text.x = theme_blank(),
	axis.text.y = theme_blank(),
	axis.ticks = theme_blank(),
	panel.grid.major = theme_blank(),
	panel.grid.minor = theme_blank(),
	plot.title = theme_text(colour="gray30", size=13))


## We can also use maps with ggmap
# get map
north_ca = get_map(location = c(lon=mean(quakes$Longitude), lat=mean(quakes$Latitude)), 
	maptype="terrain", color="bw", zoom=6)

# plot with ggmap
ggmap(north_ca) + 
geom_point(data=quakes, aes(x=Longitude, y=Latitude, size=Magnitude, colour=Magnitude), alpha=0.8) + 
labs(x="", y="") +
scale_colour_gradient(low="tomato", high="maroon") + 
opts(title = "Earthquakes in Northern California  2008 - 2011",
	axis.text.x = theme_blank(),
	axis.text.y = theme_blank(),
	axis.ticks = theme_blank(),
	plot.title = theme_text(size=12))


# get another map in colors
north_ca2 = get_map(location = c(lon=mean(quakes$Longitude), lat=mean(quakes$Latitude)), 
	maptype="terrain", zoom=6)

# plot with ggmap
ggmap(north_ca2) + 
geom_point(data=quakes, aes(x=Longitude, y=Latitude, size=Magnitude, colour=Magnitude), alpha=0.8) + 
labs(x="", y="") +
scale_colour_gradient(low="tomato", high="maroon") + 
opts(title = "Earthquakes in Northern California  2008 - 2011",
	axis.text.x = theme_blank(),
	axis.text.y = theme_blank(),
	axis.ticks = theme_blank(),
	plot.title = theme_text(size=12))

