# Analysis of the dispersal data from the lower-draw plots. 
# Data spans 2 years (2007, 2008).

rm(list=ls())
library(ggplot2)
setwd('~/Documents/Work/Active Projects/Dispersal/analysis/')
source('functions.r')
load('dispersal.rdata')
# Some cleanup of the dataset

names(dispersal)=c("loc","dist","depth","wax1","wax2","wax3","year","moisture","true_year","plot","week","trt","d1","d2","data_by")
dispersal$dist[dispersal$dist==1]=100
dispersal$dist[dispersal$dist==1.4]=140
dispersal$total=dispersal$wax1+dispersal$wax2+dispersal$wax3
d_data=dlply(dispersal, .(year))


# --------------------------------------------------------------------------------------
# Plot 1
# This shows the number of EPN we recovered from dry vs wet plots (the two columns). Each row is a week.


ggplot(d_data[[1]],aes(x=dist,y=total)) + geom_point(size=3) + facet_grid(week~trt) + ylim(1,25) + no_bg  + no_small_grid + draw_axis + scale_x_continuous(breaks=c(0,25,50,75,100,140))


# --------------------------------------------------------------------------------------
year1=subset(dispersal,year==1)
plots=dlply(year1, .(trt))

# dry plots by week
ggplot(plots[[1]],aes(x=dist,y=total)) + geom_point() + no_bg + no_small_grid + ylim(1,20) + facet_grid(~week) + opts(title="Dry Plots by week") + draw_axis

# wet plots by week
ggplot(plots[[2]],aes(x=dist,y=total)) + geom_point() + no_bg + no_small_grid + ylim(1,20) + facet_grid(~week) + opts(title="Dry Plots by week") + draw_axis

 