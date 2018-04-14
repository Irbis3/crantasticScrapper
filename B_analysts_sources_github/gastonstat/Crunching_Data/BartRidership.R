# =========================================================================
# Title:        BartRidership.R
# Author:       Gaston Sanchez
# Date:         May, 2012
# Description:  Code in R to scrape data from BART website in order to
#               get the stations coordinates, and visualize the
#               average weekly ridership using different maps
#
# License:      BSD Simplified License
#               http://www.opensource.org/license/BSD-3-Clause
#               Copyright (c) 2012, Gaston Sanchez
#               All rights reserved
# =========================================================================


# Visualizing BART Ridership Numbers 
# Bart data reports: "http://www.bart.gov/about/reports/index.aspx"
# Bart annual exits: "http://www.bart.gov/docs/BART_Ridership_FY73_FY11.xlsx"


# R packages
library(XML)
library(ggplot2)
library(reshape)
library(RgoogleMaps)
library(maps)
library(maptools)
library(ggmap)
library(mapproj)


## First we need to get the coordinates (latitude and longitude)
## for each BART station. There are several ways to get this information
## but here I will parse the data from the google maps of each station
## that appear in the BART website

# url bart stations
bart_stations = "http://www.bart.gov/stations/index.aspx"

# parse internal document
doc_stations = htmlParse(bart_stations)

# find matching nodes
nodes = getNodeSet(doc_stations, "//div/ul/li/a")

# get bart station names
st_names = sapply(nodes, function(x) xmlValue(x, "id"))
st_names = st_names[1:44]

# get hrefs
hrefs = sapply(nodes, function(x) xmlGetAttr(x, "href"))

# get bart station hrefs
href_stations = hrefs[1:44]

# get station abbreviation
st_abb = gsub("/index.aspx", "", href_stations)

# get coordinates
bart_url = "http://www.bart.gov/stations/"
lat = rep("", length(st_abb))
lon = lat
for (i in 1:length(href_stations))
{
	st_map = paste(bart_url, st_abb[i], "/neighborhood.aspx", sep="")
	# open connection in read mode
	con_aux = url(st_map, open="r")
	# read lines
	tmp = readLines(con_aux)
	# close connection
	close(con_aux)
	# where is the latitude?
	where = grep("lat : ", tmp)
	# split string by ',' comma
	where.split = strsplit(tmp[where], ",")
	# get third and fourth elements
	latitude = where.split[[1]][3]
	longitude = where.split[[1]][4]
	# store lat and lon
	lat[i] = sub(" lat : ", "", latitude)
	lon[i] = sub(" lng: ", "", longitude)
}

# convert as numeric
lat = as.numeric(lat)
lon = as.numeric(lon)

# create data frame
stations = data.frame(Name=st_names, lat=lat, lon=lon, stringsAsFactors=FALSE)


## Now we need to download the ridership data available at
## "http://www.bart.gov/about/reports/index.aspx"
## There are several reports to play with
## I downloaded the average weekday exits by stations

# import ridership data (download it first to your computer!)
wd = "/Users/BartRidership/"
datafile = "FY_Avg_Wkdy_Exits_By_Station.csv"
exits = read.csv(paste(wd, datafile, sep=""), stringsAsFactors=FALSE)

# change name of Berkeley station
exits$Station[exits$Station == "Berkeley"] = "Downtown Berkeley"

# reorder tables by station name
stations = stations[order(stations$Name),]
exits = exits[order(exits$Station),]

# add 'lat' and 'lon' to exits
exits$lat = stations$lat
exits$lon = stations$lon

# let's do a very simple plot
plot(exits$lon, exits$lat, pch=19, col=hsv(0,0,0.6,0.5), 
    cex=sqrt(exits$FY11/1000))


## Now we are ready to start visualizing the data with different maps
## First we're going to use 'RgoogleMaps'

# R Google Maps option 1 (color terrain map)
center = c(mean(exits$lat), mean(exits$lon))
zoom = min(MaxZoom(range(exits$lat), range(exits$lon)))
BayAreaMap1 = GetMap(center=center, zoom=zoom, destfile="BayAreaMap1.png")

# R Google Maps option 2 (gray hybrid map)
BayAreaMap2 = GetMap(center=center, zoom=zoom, destfile="BayAreaMap2.png", 
	GRAYSCALE=TRUE, maptype="hybrid")

# R Google Maps option 3 (mobile map)
BayAreaMap3 = GetMap.bbox(exits$lon, exits$lat, destfile="BayAreaMap3.png", 
	maptype="mobile")

# R Google Maps option 4 (gray mobile map)
BayAreaMap4 = GetMap.bbox(exits$lon, exits$lat, destfile="BayAreaMap4.png", 
	GRAYSCALE=TRUE, maptype="mobile")

# plot on map 1
dev.new()
PlotOnStaticMap(BayAreaMap1, exits$lat, exits$lon, col=hsv(0.95,1,1,0.5), 
	pch=20, cex=sqrt(exits$FY11/1000))

# plot on map 2
dev.new()
PlotOnStaticMap(BayAreaMap2, exits$lat, exits$lon, col=hsv(0.65,1,1,0.5), 
	pch=20, cex=sqrt(exits$FY11/1000))

# plot on map 3
dev.new()
PlotOnStaticMap(BayAreaMap3, exits$lat, exits$lon, col=hsv(0.95,1,1,0.5), 
	pch=20, cex=sqrt(exits$FY11/1000))

# plot on map 4
dev.new()
PlotOnStaticMap(BayAreaMap4, exits$lat, exits$lon, col=hsv(0.65,1,1,0.5), 
	pch=20, cex=sqrt(exits$FY11/1000))


## We can also use the maps provided by ggmap
## get map type 'terrain'
baymap = get_map(location = c(lon=mean(exits$lon), lat=mean(exits$lat)), 
	maptype="terrain", color="bw")

# plot with ggmap
ggmap(baymap) + 
geom_point(data=exits, aes(x=lon, y=lat, size=FY11), colour="tomato", alpha=0.8) + 
labs(x="", y="", size="Weekly \nRidership") +
opts(title = "BART Weekly ridership - 2011",
	axis.text.x = theme_blank(),
	axis.text.y = theme_blank(),
	axis.ticks = theme_blank(),
	plot.title = theme_text(size=12))


## Let's try to get a representation by years
# melt data
exm = melt(exits, id.vars=c("Station", "lat", "lon"), variable_name="Year")

# change Year labels
levels(exm$Year) = 1999:2011

# define object ggmap
gg1 = ggmap(baymap) + 
geom_point(data=subset(exm, Year!=2011), 
	aes(x=lon, y=lat, group=Year, colour=value, size=value), alpha=0.5) + 
scale_size_continuous(breaks=c(500,1000,5000,10000,15000,20000,30000), range=c(2,8)) +
facet_wrap(~ Year) +
labs(x="", y="", size="weekly \nridership") +
opts(title = "Average BART Weekly Ridership by Year",
	axis.text.x = theme_blank(),
	axis.text.y = theme_blank(),
	axis.ticks = theme_blank(),
	plot.title = theme_text(size=12))

# turn off color legend
sc = scale_colour_gradient(breaks=c(500,1000,5000,10000,15000,20000,30000), 
    low="orange", high="red3") 
sc$legend = FALSE

# plot map
gg1 + sc


## With ggmap we have the option to plot data on stamen maps
# get stamen map
sfmap = get_map(location = c(lon=mean(exits$lon), lat=mean(exits$lat)), 
	maptype="terrain", color="bw", source="stamen")

# plot map
ggmap(sfmap) + 
geom_point(data=exits, aes(x=lon, y=lat, size=sqrt(FY11/100), colour=FY11)) 


## Another feature of ggmap is the possibility to work with openstreet maps
# option with open street map
opmap = get_openstreetmap(bbox = c(left=min(exits$lon), bottom=min(exits$lat), 
	right=max(exits$lon), top=max(exits$lat)), scale=400000, color="bw")

# plot map
ggmap(opmap) + 
geom_point(data=exits, aes(x=lon, y=lat, size=FY11), colour="red", alpha=0.5) +
scale_size_continuous(range=c(3,8)) + 
labs(x="", y="", size="Weekly \nRidership") +
opts(title = "BART Stations - Average Weekly Ridership in 2011",
	axis.text.x = theme_blank(),
	axis.text.y = theme_blank(),
	axis.ticks = theme_blank(),
	plot.title = theme_text(size=12, colour="gray30"))


