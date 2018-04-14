rm(list=ls())
setwd('~/Github/postdoc/dispersal_analysis')
library(plyr)
library(ggplot2)
library(stringr)
library(reshape2)
load("data/cleaned_disp_data.rdata")


# Data summaries
s1 <- melt(disp_data, id.vars=1:7)

s2 <- dcast(subset(s1, period == "Y1"), trt + distance ~ week, mean)
s3 <- melt(s2, id.vars=1:2)
ggplot(s3, aes(distance, value, colour=trt)) + geom_point(size=3, alpha=0.8) + facet_wrap(~variable, nrow=2, ncol=3, scales = "free")
# Analyses

# Plots

