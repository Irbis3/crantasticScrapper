#Clean Homicide data
homicides$Crimetype <- NULL
homicides <- subset(homicides, Month != "Tot")
homicides$date <- as.Date(str_c(homicides$Year,
                              homicides$Month, "15", sep = "-"),
                    format("%Y-%b-%d"))
homicides$Homicides <- gsub(",", "", homicides$Homicides)
homicides$Homicides <- as.numeric(as.character(homicides$Homicides))
#Some wise guy decided to code the number of homicides in the
#future as 0 instead of NA
homicides[which(homicides$Homicides == 0 & homicides$date >= as.Date("2010-08-15")),]$Homicides <- NA

# Clean Population data and merge with homicides
mpop <- melt(states.pop, id = "State")
mpop$variable <- as.numeric(gsub("X", "", mpop$variable))
mpop <- subset(mpop, variable >= 1997 & variable <= 2010)
mpop$Month <- "Jun"
names(mpop) <- c("State", "Year", "Pop", "Month")
hom.mpop.all <- merge(homicides, mpop,
                      by = c("State", "Year", "Month"),
                      all.x = TRUE)

#data of all homicides with rates
hom <- ddply(hom.mpop.all, .(State), transform,
                  rate = Homicides / na.spline(Pop) *100000*12)

# Now only the recent data
hom.dw <- subset(hom, date >= as.Date("2004-12-15"))

# National data only
hom.nat <- subset(hom, State %in% c("National"))
#No dates after august -- incomplete data for some states
hom.nat <- subset(hom.nat, date <= as.Date("2010-08-15") &
              date >= as.Date("1990-12-15"))


important.dates$date <- as.Date(important.dates$date,
                                format = "%Y/%m/%d")

#Clean the execution data
executions$date <- seq(as.Date("2006-12-15"),
                       as.Date("2010-10-15"),
                by = "month")
executions <- merge(subset(hom.nat,
                           State == "National"),
                    executions, by = "date")
executions$diff <- executions$Reforma - executions$Milenio
mexecutions <- melt(executions[ ,c("date",
                                   "Homicides",
                                   "Reforma",
                                   "Milenio")], id="date")

