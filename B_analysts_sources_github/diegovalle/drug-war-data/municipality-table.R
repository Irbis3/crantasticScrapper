library(data.table)
library(mxmortalitydb)
library(stringr)
library(lubridate)
library(plyr)
library(testthat)
library(zoo)
library(jsonlite)
options(stringsAsFactors = FALSE)
source("functions.R")

## Only deaths that occurred in Mexico
injury.intent <- subset(injury.intent, state_occur_death %in% 1:32)
##add dates of registration and occurrence
injury.intent$date_reg <- dateReg(injury.intent, assume = TRUE)
injury.intent$date_occur <- dateOccur(injury.intent, assume = TRUE)


## Variables to modify
kmaxy <- 2012
kminy <- 2004
kpercent.change <- 27178/25757
kpercent.change <- ddply(subset(injury.intent, intent == "Homicide" & year(date_reg) == kmaxy-1), 
                         .(year(date_reg)), summarise, count = sum(length(date_reg)))$count /
  ddply(subset(injury.intent, intent == "Homicide" & year(date_reg) == kmaxy-1 &
                 year(date_occur) == kmaxy-1), 
        .(year(date_occur)), summarise, count = sum(length(date_reg)))$count

##data(deaths)
##data(injury.intent)


municipality.heads <- local({
  municipality.heads <- read.csv("data/municipality-heads.csv")
  metro <- read.csv("data/metropolitan-areas200k.csv")
  merge(municipality.heads, metro, all = TRUE)})

pop <- read.csv("data/municipio-population1990-2030.csv.bz2")
pop <- subset(pop, Sex == "Total")
names(pop) <- c("id", "Sex", "Year", "Population")
pop$Sex <- NULL



drug.homicides <- read.csv("data/drug-homicides.csv.bz2")
drug.homicides$date <- as.Date(drug.homicides$date)





## Where the mun of death is unknow use mun of registration
injury.intent[injury.intent$mun_occur_death2 == 999, ]$state_occur_death <- 
  injury.intent[injury.intent$mun_occur_death2 == 999, ]$state_reg
injury.intent[injury.intent$mun_occur_death2 == 999, ]$mun_occur_death2 <- 
  injury.intent[injury.intent$mun_occur_death2 == 999, ]$mun_reg



hom <- subset(injury.intent, intent == "Homicide" & year(date_occur) %in% 2004:kmaxy)


DT <- data.table(hom)
DT$year <- year(DT$date_occur)
DT$month <- month(DT$date_occur)
DT <- DT[, length(as.vector(intent)),
         by = list(year, month, state_occur_death, mun_occur_death2)]

DT$date <- as.Date(str_c(DT$year, DT$month, "15", sep = "-"))
DT$id <-generateId(DT$state_occur_death, DT$mun_occur_death2)


dates <- data.frame(id = rep(unique(municipality.heads$id), each = 12*(kmaxy-kminy+1)), 
                    date = seq(as.Date(str_c(2004,"-01-15")),
                               as.Date(str_c(kmaxy,"-12-15")),
                               by="month"))
DT <- as.data.frame(DT)
DT$month <- NULL
DT$year <- NULL
DT$state_occur_death <- NULL
DT$mun_occur_death2<- NULL

DT <- merge(DT, dates, by= c("date", "id"), all = TRUE)
DT$V1[is.na(DT$V1)] <- 0

DT <- merge(DT, na.omit(drug.homicides),
            all = TRUE)



DT <- merge(DT, municipality.heads, by = "id", all = TRUE)
#Add a made up metro area
DT[DT$id == 25018,]$MetroArea = "Culiacán-Navolato"
DT[DT$id == 25006,]$MetroArea = "Culiacán-Navolato"


##Prepare population
pop$date <- as.Date(str_c(pop$Year, "06", "15", sep = "-"))
pop$Year <- NULL

DT <- merge(DT, pop, by = c("id", "date"), all.x = TRUE)
# wrong.ids <- ddply(DT, .(id), 
#       summarise, 
#       pop = sum(!is.na(Population)))[which(ddply(DT, 
#                                                  .(id), 
#                                                  summarise, 
#                                                  pop = sum(!is.na(Population)))$pop == 0),]$id
# injury.intent[which(injury.intent$state_occur_death == 1 & 
#                       injury.intent$mun_occur_death == 17),]
DT <- ddply(DT, .(id), transform,
            Population = round(na.spline(Population, method = "monoH.FC")),
            .progress = "text")

ddply(DT, .(year(date)), summarise, sum(V1))

#Adjust for under-counting because of the cuttoff date of December 31, kmaxy

undercount <- ddply(subset(injury.intent, intent == "Homicide" &
                             !is.na(mun_occur_death) &
                             year(date_reg) == kmaxy &
                             year(date_occur) == kmaxy-1),
                    .(year(date_occur), month(date_occur), 
                      state_occur_death, mun_occur_death2),
                    summarise,
                    undercount = length(date_occur),
                    .progress = "text")
totalkmaxym1 <-  ddply(subset(injury.intent, intent == "Homicide" &
                                !is.na(mun_occur_death) &
                                year(date_reg) == kmaxy-1 &
                                year(date_occur) == kmaxy-1),
                       .(year(date_occur), month(date_occur), 
                         state_occur_death, mun_occur_death2),
                       summarise,
                       homicides09 = length(date_occur),
                       .progress = "text")

under <- merge(undercount,totalkmaxym1, all = TRUE )
under[is.na(under)] <- 0 
under$per <- (under$homicides09 + under$undercount) / (under$homicides09)

under$ANIODEF <- kmaxy
names(under)[1:2] <- c("year", "month")
under$date <- as.Date(str_c(under$ANIODEF, under$month, "15", sep = "-"))

under$year <- NULL
under$month <- NULL
under$ANIODEF <- NULL
under$homicides09 <- NULL
##under$per[!is.finite(under$per)] <-

##under$per <- under$undercount * kpercent.change
under$undercount <- NULL
##under[is.infinite(under$per),]

##under$per[is.infinite(under$per)] <- kpercent.change




DT <- merge(DT, under, by.y = c("date", "state_occur_death", "mun_occur_death2"),
            by.x = c("date", "CVE_ENT", "CVE_MUN"), all.x = TRUE)

names(DT) <- c("Date", "StateCode",
               "MunCode", "fips",
               "Homicides", "DWRH",
               "MunName", "Long", "Lat",
               "DistUSBorder", "MetroArea",
               "Population", "Undercount")
DT$Undercount[is.na(DT$Undercount)] <- 1
DT$Undercount[!is.finite(DT$Undercount)] <- (DT$Homicides[!is.finite(DT$Undercount)] +1) /
  DT$Homicides[!is.finite(DT$Undercount)]
DT$Undercount[!is.finite(DT$Undercount)] <- 1.5

ddply(DT, .(year(Date)), summarise, sum(Homicides))
ddply(DT, .(year(Date)), summarise, sum(ceiling(Homicides * Undercount)))




DT$StateName <- stateToAbbrev(DT$StateCode)

DT$name <- ifelse(is.na(DT$MetroArea),
                  str_c(DT$MunName, ", ", DT$StateName),
                  str_c(DT$MetroArea, ""))


###Deaths of Unspecified Intent classified as homicides
hom.imputed <- subset(injury.intent, intent.imputed == "Homicide" & 
                        year(date_occur) %in% 2004:kmaxy)
DT.imputed <- data.table(subset(hom.imputed, !is.na(mun_occur_death2)))
DT.imputed$year <- year(DT.imputed$date_occur)
DT.imputed$month <- month(DT.imputed$date_occur)
DT.imputed <- DT.imputed[, length(as.vector(intent.imputed)),
                         by = list(year, month, state_occur_death, mun_occur_death2)]

DT.imputed$date <- as.Date(str_c(DT.imputed$year, DT.imputed$month, "15", sep = "-"))
DT.imputed$id <- as.numeric(gsub(" ", "0",
                                 str_c(format(DT.imputed$state_occur_death, width = 2),
                                       format(DT.imputed$mun_occur_death2, width = 3))))
DT.imputed$month <- NULL
DT.imputed$year <- NULL
DT.imputed$state_occur_death <- NULL
DT.imputed$mun_occur_death2<- NULL

DT.imputed <- as.data.frame(DT.imputed)
names(DT.imputed) <- c("Homicides.Imputed", "Date", "fips")

ddply(DT.imputed, .(year(Date)), summarise, count = sum(Homicides.Imputed))

DT <- merge(DT, DT.imputed, by= c("Date", "fips"), all = TRUE)
DT$Homicides.Imputed[is.na(DT$Homicides.Imputed)] <- 0


DT$Homicides.No.Undercount <- ceiling(DT$Homicides * DT$Undercount)
DT$Homicides.Imputed.No.Undercount <- ceiling(DT$Homicides.Imputed * DT$Undercount)

ddply(DT, .(year(Date)), summarise, sum(Homicides.No.Undercount))
ddply(DT, .(year(Date)), summarise, sum(Homicides.Imputed.No.Undercount))

DT <- DT[order(DT$fips, DT$Date),]

DT$Homicides.Imputed[which(DT$name == "San Fernando, Tamps" &
                              DT$Date == as.Date("2011-04-15"))] <- 202
DT$Homicides.Imputed[which(DT$name == "Taxco de Alarcón, Gro" &
                              DT$Date == as.Date("2010-05-15"))] <- 58

DT$Homicides.Imputed.No.Undercount[which(DT$name == "San Fernando, Tamps" &
                                            DT$Date == as.Date("2011-04-15"))] <- 202
DT$Homicides.Imputed.No.Undercount[which(DT$name == "Taxco de Alarcón, Gro" &
                                           DT$Date == as.Date("2010-05-15"))] <- 58



write.csv(DT,
          "clean-data/homicides-dwrh-month-municipality.csv",
          row.names = FALSE, na = "")


web <- data.table(DT)

web <- web[,  list(hom = sum(Homicides.No.Undercount),
                   hom.imputed = sum(Homicides.Imputed.No.Undercount),
                   drh = sum(DWRH),
                   pop = sum(Population),
                   lat = mean(Lat),
                   long = mean(Long)),
           by = list(name, Date)]
web <- as.data.frame(web)



web$homrate <- round(((web$hom / numberOfDays(web$Date)) * 30) / web$pop * 10^5 * 12,1)
web$hom.imputedrate <- round(((web$hom.imputed / numberOfDays(web$Date)) * 30) / web$pop * 10^5 * 12,1)
web$drhrate <- round(((web$drh / numberOfDays(web$Date)) * 30) / web$pop * 10^5 * 12,1)
web$lat <- round(web$lat, 5)
web$long <- round(web$long, 5)

## encoding = windows-1252
## otherwise cartodb can't figure out the encoding
write.csv(web,
          "clean-data/homicides-web.csv",
          row.names = FALSE, na = "",
          fileEncoding = "windows-1252")


## write json for google-maps.js
toJSON(ddply(web, 
             .(year(Date), month(Date)),
             summarise,
             count = sum(hom))$count)

toJSON(ddply(web, 
             .(year(Date), month(Date)),
             summarise,
             count = sum(hom.imputed))$count)

toJSON(ddply(web, 
             .(year(Date), month(Date)),
             summarise,
             pop= sum(pop))$pop)
