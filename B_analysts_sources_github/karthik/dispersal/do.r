# Analysis of the dispersal data from the lower-draw plots. 
# Data spans 2 years (2007, 2008).

rm(list=ls())
library(ggplot2)
setwd('~/Documents/Work/Active Projects/Dispersal/analysis/')
source('functions.r')
load('dispersal.rdata')
# Some cleanup of the dataset
dispersal$dist[dispersal$dist==1]=100
dispersal$dist[dispersal$dist==1.4]=140
names(dispersal)=c("loc","dist","depth","wax1","wax2","wax3","year","moisture","true_year","plot","week","trt","d1","d2","data_by")
dispersal$total=dispersal$wax1+dispersal$wax2+dispersal$wax3
d_data=dlply(dispersal, .(year))


# --------------------------------------------------------------------------------------
# Plot 1
# This shows the number of EPN we recovered from dry vs wet plots (the two columns). Each row is a week.


ggplot(d_data[[1]],aes(x=dist,y=total)) + geom_point(size=3) + facet_grid(week~trt) + ylim(1,25) + no_bg  + no_small_grid + draw_axis + scale_x_continuous(breaks=c(0,25,50,75,100,140))




# --------------------------------------------------------------------------------------


y1=data.frame(year1$Location,year1$Distance,year1$Depth,year1$True_Year,year1$Plot, year1$Week,year1$Treatment,year1$total)
names(y1)= c("loc","dist","depth","yr","plot","week","trt","total")

# ggplot(y1,aes(x=total)) + geom_histogram() + facet_grid(plot ~ trt)


y2=data.frame(year2$Location,year2$Distance,year2$Depth,year2$True_Year,year2$Plot, year2$Week,year2$Treatment,year2$total)
names(y2)= c("loc","dist","depth","yr","plot","week","trt","total")

# summ = melt (sid4a_temp, id.vars="year")
# summ2 = cast( year ~ variable, data= summ, fun=mean)

# Year 1
y1=data.frame(y1$dist,y1$depth,y1$plot,y1$trt,y1$week,y1$total)
names(y1)= c("dist","depth","plot","trt","week","total")

w1=subset(y1, y1$week==1)
w1=data.frame(w1_wet$dist,w1$wet$depth,w1_wet$total)
w1_wet= subset(w1, w1$trt=="wet")
w1_dry= subset(w1, w1$trt=="dry")
ggplot(data=w1,aes(x=dist,y=total)) + geom_point() + opts(title="Week 1") + facet_grid (trt ~ plot)

## W2

w2=subset(y1, y1$week==2)
w2_wet= subset(w1, w1$trt=="wet")
w2_dry= subset(w1, w1$trt=="dry")
ggplot(data=w1,aes(x=dist,y=total)) + geom_point() + opts(title="Week 2") + facet_grid (trt ~ plot)

## W3

w3=subset(y1, y1$week==3)
w3_wet= subset(w1, w1$trt=="wet")
w3_dry= subset(w1, w1$trt=="dry")
ggplot(data=w1,aes(x=dist,y=total)) + geom_point() + opts(title="Week 3") + facet_grid (trt ~ plot)


## W4

w4=subset(y1, y1$week==4)
w4_wet= subset(w1, w1$trt=="wet")
w4_dry= subset(w1, w1$trt=="dry")
ggplot(data=w1,aes(x=dist,y=total)) + geom_point() + opts(title="Week 4") + facet_grid (trt ~ plot)


# W5
w5=subset(y1, y1$week==5)
w5_wet= subset(w1, w1$trt=="wet")
w5_dry= subset(w1, w1$trt=="dry")
ggplot(data=w5,aes(x=dist,y=total)) + geom_point() + opts(title="Week 5") + facet_grid (trt ~ plot)


# W6
w6=subset(y1, y1$week==6)
w6_wet= subset(w1, w1$trt=="wet")
w6_dry= subset(w1, w1$trt=="dry")
ggplot(data=w1,aes(x=dist,y=total)) + geom_point() + opts(title="Week 6") + facet_grid (trt ~ plot)

#

# Extinction Rate Wet#

y1_wet = subset(y1,y1$trt=="wet")
y1ws= data.frame(y1_wet$dist, y1_wet$week, y1_wet$total)
names(y1ws)=c("dist","week","total")
y1ws2=melt(y1ws, id.vars=1:2)

y1wss = cast(y1ws2,  week + dist ~ variable, fun= sum)
fraction = y1wss$total/100000;
y1wss= data.frame(y1wss,fraction)
ggplot(data=y1wss,aes(x=dist,y=fraction)) + geom_point() + facet_grid(week ~.) + geom_line() + opts(title="Fraction recovered from wet plots")



# Extinction Rate Dry#

y1_wet = subset(y1,y1$trt=="dry")
y1ws= data.frame(y1_wet$dist, y1_wet$week, y1_wet$total)
names(y1ws)=c("dist","week","total")
y1ws2=melt(y1ws, id.vars=1:2)

y1wss = cast(y1ws2,  week + dist ~ variable, fun= sum)
fraction = y1wss$total/100000;
y1wss= data.frame(y1wss,fraction)
ggplot(data=y1wss,aes(x=dist,y=fraction)) + geom_point() + facet_grid(week ~.) + geom_line() + opts(title="Fraction recovered from dry plots")


fitdistr(w1_wet$total,densfun=dweibull,start=list(scale=1,shape=1))
fitdistr(w2_wet$total,densfun=dweibull,start=list(scale=1,shape=1))
fitdistr(w3_wet$total,densfun=dweibull,start=list(scale=1,shape=1))
fitdistr(w4_wet$total,densfun=dweibull,start=list(scale=1,shape=1))
fitdistr(w5_wet$total,densfun=dweibull,start=list(scale=1,shape=1))
fitdistr(w6_wet$total,densfun=dweibull,start=list(scale=1,shape=1))

##
# DEPTH BY TRT 1-6; FIGURE 2

a= ggplot(y1w1,aes(x=week,y=depth)) + geom_point(aes(size=total)) + ylim(20,0) + my_scales
b= ggplot(y1w6,aes(x=week,y=depth)) + geom_point(aes(size=total)) + ylim(20,0) + my_scales
c = ggplot(y1d1,aes(x=week,y=depth)) + geom_point(aes(size=total)) + ylim(20,0) + my_scales
d = ggplot(y1d6,aes(x=week,y=depth)) + geom_point(aes(size=total)) + ylim(20,0) + my_scales
grid.newpage() 
pushViewport(viewport(layout = grid.layout(2, 2)))
vplayout <- function(x, y) 
viewport(layout.pos.row = x, layout.pos.col = y)
print(a, vp = vplayout(1, 1)) 
print(b, vp = vplayout(1, 2))
print(c, vp = vplayout(2, 1))
print(d, vp = vplayout(2, 2))


###############################

## Figure 3, distance x total, week1 and week 6
# either get rid of the zeroes or a hollow symbol
a= ggplot(y1w1,aes(x=distances,y=sum(total))) + geom_point(aes(size=total))  + my_scales
b= ggplot(y1w6,aes(x=distances,y=sum(total))) + geom_point(aes(size=total)) + ylab("")  + my_scales
c = ggplot(y1d1,aes(x=distances,y=sum(total))) + geom_point(aes(size=total))  + my_scales
d = ggplot(y1d6,aes(x=distances,y=sum(y1d_total.1))) + geom_point(aes(size=total)) + ylab("")   + my_scales

grid.newpage() 
pushViewport(viewport(layout = grid.layout(2, 2)))
vplayout <- function(x, y) 
viewport(layout.pos.row = x, layout.pos.col = y)
print(a, vp = vplayout(1, 1)) 
print(b, vp = vplayout(1, 2))
print(c, vp = vplayout(2, 1))
print(d, vp = vplayout(2, 2))




resulting_data = dlply(year1, .(plot), agg)
# This makes a count of each plot ignoring depth and direction

r=ldply(resulting_data)
ggplot(r, aes(dist,count,group=plot)) + geom_line() + facet_wrap(trt~week)


# six weeks, 4 plots per week. so 24 plots in year1.

