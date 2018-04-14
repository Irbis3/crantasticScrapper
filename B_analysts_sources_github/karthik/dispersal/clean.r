# New effort to wrap up my dispersal experiment.
rm(list=ls())
setwd('~/Github/postdoc/dispersal_analysis')
library(stringr)
load("data/raw_dispersal.rdata")

names(year1) <- tolower(names(year1))
year1 <- year1[,-which(names(year1)=="total")]
names(year2) <- tolower(names(year2))
year1 <- year1[, -which(names(year1)=="year")]
year2 <- year2[, -which(names(year2)=="year")]
year1$period <- "Y1"
year2$period <- "Y2"
disp_data <- rbind(year1, year2)

# Cleaning up field names
disp_data$location <- str_trim(tolower(disp_data$location), side = "both")
disp_data$total_hits <- disp_data$g_w1 + disp_data$g_w2 + disp_data$g_w3
names(disp_data)[which(names(disp_data)=="treatment")]="trt"
# Clean up dates
disp_data$d1 <- as.Date(disp_data$d1)
disp_data$d2 <- as.Date(disp_data$d2)
disp_data$distance <- round(disp_data$distance,0)
disp_data$trt <- as.factor(disp_data$trt)
disp_data$week <- as.factor(disp_data$week)

# disp_data dimensions = 5328, 16
disp_data <- disp_data[, -c(4,5,6,12,13,14)]
# temporarily removing moisture field as well.
disp_data <- disp_data[, -4]
# rearranging columns
disp_data <- disp_data[, c(7,6,5,2,3,1,4,8,9)]

# fix distances
disp_data$distance[disp_data$distance==1]=100
disp_data$distance[disp_data$distance==1.4]=140

# remove true year temporarily
disp_data <- disp_data[, -which(names(disp_data)=="true_year")]

# save cleaned data to disk
save(disp_data, file="data/cleaned_disp_data.rdata")
