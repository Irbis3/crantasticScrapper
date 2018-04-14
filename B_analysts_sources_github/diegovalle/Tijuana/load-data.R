kmaxy <- 2009
kminy <- 2004
last.day <- as.Date(str_c(kmaxy, "12", "31", sep = "-"))
last.year <- 2009

deaths <- read.csv("data/deaths.bc.csv.bz2", fileEncoding = "UTF-8")
hom <- subset(deaths, PRESUNTOtxt == "Homicide")
accidents.tj <- subset(deaths, PRESUNTOtxt == "Accident" & MA == "Tijuana")
suicides.tj <- subset(deaths, PRESUNTOtxt == "Suicide" & MA == "Tijuana")
drh <- read.csv("data/drug-homicides-monthly-ma.csv.bz2", fileEncoding = "UTF-8")
drh.tj <- subset(drh, MA == "Tijuana")
