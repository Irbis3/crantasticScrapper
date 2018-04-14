# Set working directory, load data, load libraries
# setwd("C:/path/to/directory") # Uncomment to automatically set working directory
obes <- read.csv("obes.csv") # Source data from http://stateofobesity.org/adult-obesity/
library(ggplot2)
library(reshape2)
library(viridis)

# Do some messing around with formatting:
obgrid<-melt(obes, id=c("state", "reg.bea", "reg.cens"))
names(obgrid)<-c("state", "reg.bea", "reg.cens","year","ob.rate")
obgrid$year<-as.character(obgrid$year)  # Convert "year" factors to strings

# Change our "ob.0x" notation to a numeric year.
obgrid$year[obgrid$year=="ob.03"]<-2003
obgrid$year[obgrid$year=="ob.04"]<-2004
obgrid$year[obgrid$year=="ob.05"]<-2005
obgrid$year[obgrid$year=="ob.06"]<-2006
obgrid$year[obgrid$year=="ob.07"]<-2007
obgrid$year[obgrid$year=="ob.08"]<-2008
obgrid$year[obgrid$year=="ob.09"]<-2009
obgrid$year[obgrid$year=="ob.10"]<-2010
obgrid$year[obgrid$year=="ob.11"]<-2011
obgrid$year[obgrid$year=="ob.12"]<-2012
obgrid$year[obgrid$year=="ob.13"]<-2013
obgrid$year[obgrid$year=="ob.14"]<-2014
obgrid$year[obgrid$year=="ob.15"]<-2015

# convert "year" column to numeric
obgrid$year<-as.numeric(obgrid$year)
# Reorder states to show the most obese at top
obgrid$state<-reorder(subset(obgrid, year==2015)$state, subset(obgrid, year==2015)$ob.rate)

ggplot(obgrid, aes(year, state))+
  geom_tile(aes(fill=ob.rate))+
  scale_x_continuous(breaks=2003:2015)+
  scale_fill_viridis(option="inferno", labels=scales::percent)+
  labs(title="Adult Obesity Rates by State",
       subtitle="States ordered by BEA Region, then by 2015 obesity rate",
       x="Year",
       y="",
       fill="Obesity Rate",
       caption="created by /u/zonination")+
  facet_grid(reg.bea~., scales="free_y", space="free_y")+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust=.5))
ggsave("obes.png", height=12, width=8, dpi=120, type="cairo-png")