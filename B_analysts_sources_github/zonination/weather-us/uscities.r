# Scrape data from Wundergrond (this may take a few hours)
# source("chicago.R")
# source("losangeles.R")
# source("boston.R")
# source("seattle.R")
# source("houston.R")
# source("denver.R")
# source("anchorage.R")
# source("honolulu.R")
# source("minneapolis.R")
# source("phoenix.R")
# source("billings.R")
# source("topeka.R")
# source("elpaso.R")
# source("saltlake.R")
# source("miami.R")
# source("sanfrancisco.R")
# source("cleveland.R")
# source("charleston.R")
# source("nashville.R")
# source("okcity.R")
# source("portland.R")
# source("baltimore.R")
# source("stlouis.R")
# source("bismarck.R")

weather<-read.csv("chicago.csv", header=TRUE, stringsAsFactors=FALSE)
weather<-rbind(read.csv("losangeles.csv", header=TRUE, stringsAsFactors=FALSE),weather)
weather<-rbind(read.csv("boston.csv", header=TRUE, stringsAsFactors=FALSE),weather)
weather<-rbind(read.csv("seattle.csv", header=TRUE, stringsAsFactors=FALSE),weather)
weather<-rbind(read.csv("houston.csv", header=TRUE, stringsAsFactors=FALSE),weather)
weather<-rbind(read.csv("denver.csv", header=TRUE, stringsAsFactors=FALSE),weather)
weather<-rbind(read.csv("anchorage.csv", header=TRUE, stringsAsFactors=FALSE),weather)
weather<-rbind(read.csv("honolulu.csv", header=TRUE, stringsAsFactors=FALSE),weather)
weather<-rbind(read.csv("minneapolis.csv", header=TRUE, stringsAsFactors=FALSE),weather)
weather<-rbind(read.csv("phoenix.csv", header=TRUE, stringsAsFactors=FALSE),weather)
weather<-rbind(read.csv("billings.csv", header=TRUE, stringsAsFactors=FALSE),weather)
weather<-rbind(read.csv("topeka.csv", header=TRUE, stringsAsFactors=FALSE),weather)
weather<-rbind(read.csv("elpaso.csv", header=TRUE, stringsAsFactors=FALSE),weather)
weather<-rbind(read.csv("saltlake.csv", header=TRUE, stringsAsFactors=FALSE),weather)
weather<-rbind(read.csv("miami.csv", header=TRUE, stringsAsFactors=FALSE),weather)
weather<-rbind(read.csv("sanfrancisco.csv", header=TRUE, stringsAsFactors=FALSE),weather)
weather<-rbind(read.csv("cleveland.csv", header=TRUE, stringsAsFactors=FALSE),weather)
weather<-rbind(read.csv("charleston.csv", header=TRUE, stringsAsFactors=FALSE),weather)
weather<-rbind(read.csv("nashville.csv", header=TRUE, stringsAsFactors=FALSE),weather)
weather<-rbind(read.csv("okcity.csv", header=TRUE, stringsAsFactors=FALSE),weather)
weather<-rbind(read.csv("portland.csv", header=TRUE, stringsAsFactors=FALSE),weather)
weather<-rbind(read.csv("baltimore.csv", header=TRUE, stringsAsFactors=FALSE),weather)
weather<-rbind(read.csv("stlouis.csv", header=TRUE, stringsAsFactors=FALSE),weather)
weather<-rbind(read.csv("bismarck.csv", header=TRUE, stringsAsFactors=FALSE),weather)

# There's some kind of fuckery going on with some cities. They decided that 
# instead of putting "NA" or leaving the date blank, it would be
# prudent to populate it with "-99999". Thanks a lot, jerks.

weather$Mean.TemperatureF<-as.numeric(weather$Mean.TemperatureF)
weather<-subset(weather,Mean.TemperatureF>=-99)