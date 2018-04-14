# New effort to wrap up my dispersal experiment.
# Analysis of the dispersal data from the lower-draw plots.
# Data spans 2 years (2007, 2008).
rm(list=ls())
setwd('~/Github/postdoc/dispersal_analysis')
library(RMySQL)

# Grabbing data from the database
con<-dbConnect(MySQL(),user="root",password=getOption("dbpass"),dbname="nematode",host="localhost")
year1=dbGetQuery(con,"select * from year1")
year2=dbGetQuery(con,"select * from year2")
# Sinking raw, unmanipulated data to disk.
save(year1, year2, file = "data/raw_dispersal.rdata")
