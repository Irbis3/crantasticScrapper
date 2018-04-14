########################################################
#####       Author: Diego Valle Jones              #####
#####       Website: www.diegovalle.net            #####
#####       Date: 2010-Jan-22                      #####
########################################################
#The homicide, suicide and accident rates according to the INEGI


#Data file with all homicides, suicides and accidents *registered* in Mexico in a given year
#data source: Estad�sticas Vitales INEGI

#Note: If you don't break down the deaths by place of occurence you'll
#end up counting deaths that took place in foreign countries but
#were registered in Mexico. Yeah, the INEGI database is hard to use.
deaths <- read.csv(bzfile("accidents-homicides-suicides/data/accidents-homicides-suicides-bystate.csv.bz2"), skip=3)
names(deaths)[1:4] <- c("Code","State","Year","Type.of.Death")
deaths <- subset(deaths, Type.of.Death != "Se ignora" &
                 State != "Extranjero" & State != "Total" &
                 Year != "No especificado" & Year !="Total")
deaths$Year <- as.numeric(as.numeric(gsub('[[:alpha:]]', '',
                     deaths$Year)))
deaths <- subset(deaths, Year >= 1990 & Year)
deaths[is.na(deaths)] <- 0
deaths$Tot <- apply(deaths[,5:ncol(deaths)], 1, function(x) sum(x))
deaths$State <- iconv(deaths$State, "windows-1252", "utf-8")

#write the total murders by state to a file
st <- cast(subset(deaths, Type.of.Death == "Homicidio"), State ~ Year, , value = "Tot")
write.csv(st, "accidents-homicides-suicides/output/states.csv", row.names = FALSE)

deaths <- ddply(deaths, .(Year, Type.of.Death), function(df) sum(df$Tot))
names(deaths) <- c("Year","Type.of.Death","Tot")


#Population of Mexico 1990-2008
#source: CONAPO
pop.mex <- c(83971014, 85583336, 87184832, 88752014, 90265775,
91724528, 93130089, 94478046, 95790135, 97114831, 98438557,
99715527, 100909374, 101999555, 103001867, 103946866,
104874282, 105790725, 106682518)

plotDeaths <- function(df, type, filename) {
    df <- subset(deaths, Type.of.Death == type)
    df$pop.mex <- pop.mex
    df$rate <- df$Tot / df$pop.mex * 100000
    write.csv(df, paste("accidents-homicides-suicides/output/", filename, sep=""))
    qplot(1990:2008, df$rate, geom="line")
}

Cairo(file="accidents-homicides-suicides/output/homicide.png")
print(plotDeaths(deaths, "Homicidio", "homicide.csv"))
dev.off()

Cairo(file="accidents-homicides-suicides/output/suicide.png")
print(plotDeaths(hom, "Suicidio", "suicide.csv"))
dev.off()

Cairo(file="accidents-homicides-suicides/output/accident.png")
print(plotDeaths(hom, "Accidente", "accident.csv"))
dev.off()



########################################################
#There were a lot of murders registered with no year
#of occurence before 1994
########################################################
ns <- read.csv(bzfile("accidents-homicides-suicides/data/accidents-homicides-suicides-bystate.csv.bz2"), skip=3)
names(ns)[1:4] <- c("Code","State","Year","Type.of.Death")
ns <- subset(ns, Type.of.Death == "Homicidio" &
                 State != "Extranjero" & State == "Total" &
                 Year == "No especificado" & Year !="Total")
ns[is.na(ns)] <- 0
print(qplot(1990:2008, unlist(ns[1,5:length(ns)]), geom = "line") +
    xlab("Year Registered") +
    ylab("Number of homicides without a year of occurrence") +
    opts(title = "There were a lot of murders registered with no year of occurence before 1994"))
dev.print(png, "accidents-homicides-suicides/output/noyear.png",
          width = 600, height = 400)

#bu state
ns <- read.csv(bzfile("accidents-homicides-suicides/data/accidents-homicides-suicides-bystate.csv.bz2"), skip=3)
names(ns)[1:4] <- c("Code","State","Year","Type.of.Death")
ns <- subset(ns, Type.of.Death == "Homicidio" &
                 State != "Extranjero" & State != "Total" &
                 Year == "No especificado" & Year !="Total")
