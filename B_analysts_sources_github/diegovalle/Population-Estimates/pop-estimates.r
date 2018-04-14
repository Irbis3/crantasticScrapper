########################################################
#####       Author: Diego Valle Jones
#####       Website: www.diegovalle.net
#####       Date Created: Sat Jan 23 21:13:21 2010
########################################################
#Population estimates for Mexico at the County level


library(reshape)
library(stringr)
library(ggplot2)
library(zoo)
library(gdata)
library(car)
options(stringsAsFactors=FALSE)

cleanCensusData <- function(filename, sex) {
  c10a <- read.csv(filename, fileEncoding = "windows-1252",
                   skip = 4)
  c10a <- c10a[-grep("=CONCA*", c10a$X),]
  c10a$X <- as.numeric(gsub(" ", "", c10a$X))
  coltoconv <- 3:ncol(c10a)
  c10a <- na.omit(c10a)
  c10a[,coltoconv] <- sapply(c10a[,coltoconv], function(x) as.numeric(gsub(",", "", x)))
  
                                        #c10a$De.0.a.4.años <- NULL
  c10a$X0.Años <- c10a$X1.Año <- c10a$X2.Años <- c10a$X3.Años <- c10a$X4.Años <- NULL

  #c10a$No.especificado[is.na(c10a$No.especificado)] <- 0
  c10a[is.na(c10a)] <- 0
  
  coltoconv <- 4:ncol(c10a)
  adj <- c10a$Total
  c10a[,coltoconv] <- sapply(c10a[,coltoconv],
                             function(x) round(x + ((x / c10a$Total) * c10a$No.especificado)))
  c10a$No.especificado <- NULL
  colnames <- c("0.4", "5.9", "10.14", "15.19", "20.24",
                "25.29", "30.34", "35.39", "40.44", "45.49", "50.54",
                "55.59", "60.64", "65.69","70.74","75.79","80.84",
                "85plus")
  if(sex == "Males")
    colnames <- str_c("m", colnames)
  else if(sex == "Females")
    colnames <- str_c("f", colnames)
  else if(sex == "All")
    colnames <- str_c("a", colnames)
  else 
    stop()
  
  names(c10a) <- c("id", "MunName", "Population", colnames)
  head(c10a)

  
  coltoconv <- 3:ncol(c10a)
  #Tulum was created from Solidaridad
  c10a[which(c10a$id == 23008),coltoconv] <- c10a[which(c10a$id == 23009), coltoconv] + c10a[which(c10a$id == 23008), coltoconv]
  #San Ignacio Cerro Gordo was created from Arandas
  c10a[which(c10a$id == 14008), coltoconv] <- c10a[which(c10a$id == 14008), coltoconv] + c10a[which(c10a$id == 14125), coltoconv]
  c10a <- subset(c10a, !id %in% c(14125, 23009))
  c10a$Year <- 2010
  
  c10a
}

makePopEstimates <- function(pop, census10, sex) {
  funnames <- c("id", "MunName", "Population", "a.0", "a1.4", "a5.9", "a10.14", 
                  "a15.19", "a20.24", "a25.29", "a30.34", "a35.39", "a40.44", "a45.49", 
                  "a50.54", "a55.59", "a60.64", "a65.69", "a70.74", "a75.79", "a80.84", 
                  "a85.89", "a90.94", "a95.99", "a.100", "Year")
  names(pop) <- funnames

  colnames <- names(census10)
  
  pop$a.0 <- pop$a1.4 <- pop$a85.89 <- pop$a90.94 <- pop$a95.99 <- pop$a.100 <- NULL
  pop$a0.4 <- pop$a.0 + pop$a1.4
  pop$a85plus <- pop$a85.89 + pop$a90.94 +  pop$a95.99 + pop$a.100
 
  #funnames <- names(pop)
  pop <- pop[,c(1:3,21,4:19,22,20)]
  
  names(pop) <- names(census10)
  pop <- rbind(census10, subset(pop, Year != 2010))
  names(pop) <- c("id", "MunName", "Population", "a0.4", "a5.9",
                  "a10.14", "a15.19", "a20.24",
                  "a25.29", "a30.34", "a35.39", "a40.44", "a45.49", "a50.54",
                  "a55.59", "a60.64", "a65.69","a70.74","a75.79","a80.84",
                  "a85plus", "Year")
  
  
  pop[which(pop$Year %in% c(2001:2009, 2011:2012)), 3:(ncol(pop)-1)] <- NA
  pop <- pop[order(pop$Year, pop$id),]

  pop <- ddply(pop, .(id), transform,
         Population = round(na.spline(Population, method = "monoH.FC")),
         a0.4 = round(na.spline(a0.4, method = "monoH.FC")),
         a5.9 = round(na.spline(a5.9, method = "monoH.FC")),
         a10.14 = round(na.spline(a10.14, method = "monoH.FC")),
         a15.19 = round(na.spline(a15.19, method = "monoH.FC")),
         a20.24 = round(na.spline(a20.24, method = "monoH.FC")),
         a25.29 = round(na.spline(a25.29, method = "monoH.FC")),
         a30.34 = round(na.spline(a30.34, method = "monoH.FC")),
         a35.39 = round(na.spline(a35.39, method = "monoH.FC")),
         a40.44 = round(na.spline(a40.44, method = "monoH.FC")),
         a45.49 = round(na.spline(a45.49, method = "monoH.FC")),
         a50.54 = round(na.spline(a50.54, method = "monoH.FC")),
         a55.59 = round(na.spline(a55.59, method = "monoH.FC")),
         a60.64 = round(na.spline(a60.64, method = "monoH.FC")),
         a65.69 = round(na.spline(a65.69, method = "monoH.FC")),
         a70.74 = round(na.spline(a70.74, method = "monoH.FC")),
         a75.79 = round(na.spline(a75.79, method = "monoH.FC")),
         a80.84 = round(na.spline(a80.84, method = "monoH.FC")),
         a85plus = round(na.spline(a85plus, method = "monoH.FC"))
                )
  
  pop$MunName <- rep(pop$MunName[which(index(pop$MunName) %% 23 == 0)], each = 23)
  names(pop) <- colnames
  pop[,3:(ncol(pop)-1)][pop[,3:(ncol(pop)-1)] < 0] <- 0
  pop
}

simpleEstimates <- function(df, dfwomen, dfmen, censusfile) {
  pop <- merge(df, dfmen, by = c("id", "MunName", "Year"))
  pop <- merge(pop, dfwomen, by = c("id", "MunName", "Year"))
  
  pop <- pop[ , c("id", "MunName", "Year", "Population.x", "Population.y", "Population")]
  names(pop) <- c("id", "MunName", "Year", "Population", "MalePop", "FemalePop")

  ct2010 <- read.csv(censusfile)
  ct2010$Year <- 2010

  ct2010$State <- ct2010$NOM_ENT <- NULL
  ct2010 <- subset(ct2010, MUN != 0 & ENTIDAD != 0)
  names(ct2010) <- c("ENTIDAD", "MUN", "County10", "Total10", "Hombres10",
                     "Mujeres10", "Year10")

  ct2010$id <- as.numeric(gsub(" ", "0", str_c(format(ct2010$ENT, width = 2),
                                               format(ct2010$MUN, width = 3))))

  names(ct2010) <- c("ENT", "MUN", "MunName", "Population", "MalePop",
                     "FemalePop", "Year", "id")

  ct2010$ENT <- ct2010$MUN <- NULL

  ##Tulum was created from Solidaridad
  ct2010[which(ct2010$id == 23008), 2:4] <- ct2010[which(ct2010$id == 23009), 2:4] + ct2010[which(ct2010$id == 23008), 2:4]
  ##San Ignacio Cerro Gordo was created from Arandas
  ct2010[which(ct2010$id == 14008), 2:4] <- ct2010[which(ct2010$id == 14008), 2:4] + ct2010[which(ct2010$id == 14125), 2:4]
  ct2010 <- subset(ct2010, !id %in% c(14125, 23009))

  pop <- subset(pop, Year != 2010)
  pop[which(pop$Year %in% c(2001:2009, 2011:2012)), c("Population","MalePop","FemalePop")] <- NA
  pop <- rbind(ct2010, pop)
  pop <- pop[order(pop$id, pop$Year), ]

  pop <- ddply(pop, .(id), transform,
               Population = round(na.spline(Population, method = "monoH.FC")))
  pop <- ddply(pop, .(id), transform,
               MalePop = round(na.spline(MalePop, method = "monoH.FC")))
  pop <- ddply(pop, .(id), transform,
               FemalePop  = round(na.spline(FemalePop, method = "monoH.FC")))
  
  pop$MunName <- rep(pop$MunName[which(index(pop$MunName) %% 23 == 0)], each = 23)
  #make sure the municipalities match
  stopifnot(all.equal(subset(pop, Year == 1990)$id, ct2010$id))
  pop
}

CleanXLS <- function(year, sex, sheet) {
  colnames <- c(".0", "1.4", "5.9", "10.14", "15.19", "20.24",
                       "25.29", "30.34", "35.39", "40.44", "45.49", "50.54",
                       "55.59", "60.64", "65.69","70.74","75.79","80.84",
                       "85.89","90.94","95.99",".100")
  if(sex == "Males")
    colnames <- str_c("m", colnames)
  else if(sex == "Females")
    colnames <- str_c("f", colnames)
  else if(sex == "All")
    colnames <- str_c("a", colnames)
  else 
    stop()
    
  all <- read.xls(str_c("colmex/", year, "total.xls"), sheet = sheet,
                pattern = "Aguascalientes", fileEncoding = "windows-1252")
  all <- all[,c(1,2,3, 5:26)]
  names(all) <- c("id", "MunName", "Population", colnames)
  all$MunName <- iconv(all$MunName, from = "windows-1252", to = "UTF-8")
  all$id <- as.numeric(all$id)
  all <- subset(all, !is.na(all$id))
  all$Year <- year
  all[,3:ncol(all)] <- apply(all[,3:ncol(all)], 2,
                            function(x) as.numeric(gsub(",", "", x)))
  all
}

renameVariables <- function(df) {
  df$variable <- car::recode(df$variable,
            "'Population' = 'Total';
             'a0.4' = '0-4';
             'a5.9' = '5-9';
             'a10.14' = '10-14';
             'a15.19' = '15-19';
             'a20.24' = '20-24'; 
             'a25.29' = '25-29';
             'a30.34' = '30-34';
             'a35.39' = '35-39';
             'a40.44' = '40-44';
             'a45.49' = '45-49';
             'a50.54' = '50-54';
             'a55.59' = '55-59'; 
             'a60.64' = '60-64';
             'a65.69' = '65-69';
             'a70.74' = '70-74';
             'a75.79' = '75-79';
             'a80.84' = '80-84';
             'a85plus' = '85plus';")
  df
}

########################################################
#Clean the CONAPO-Colmex pop estimates
########################################################

if(!file.exists("clean-data/pop.csv") |
   !file.exists("clean-data/popmen.csv") |
   !file.exists("clean-data/popwomen.csv")) {
  
  pop <- popwomen <- popmen <- data.frame()

  for(year in 1990:2012) {
    all <- CleanXLS(year, "All", 1)
    pop <- rbind(pop, all)
    
    men <- CleanXLS(year, "Males", 2)
    popmen <- rbind(popmen, men)
    
    women <- CleanXLS(year, "Females", 3)
    popwomen <- rbind(popwomen, women)
    
    print(year)
  }
  write.csv(pop, "clean-data/pop.csv", row.names = FALSE)
  write.csv(popmen, "clean-data/popmen.csv", row.names = FALSE)
  write.csv(popwomen, "clean-data/popwomen.csv", row.names = FALSE)

} else {
  pop <- read.csv("clean-data/pop.csv")
  popmen <- read.csv("clean-data/popmen.csv")
  popwomen <- read.csv("clean-data/popwomen.csv")
}

########################################################
#There was a big error in the official population projections
#New estimates based on the 2010 Census
########################################################



pop.estimate <- simpleEstimates(pop, popwomen, popmen, "data/inegi2010.csv")
write.csv(pop.estimate, "clean-data/pop-estimates.csv", row.names = FALSE)

#Check if there are crazy numbers
ddply(pop.estimate, .(Year), summarise, sum(Population))


########################################################
#Spline Interpolation taking into account the 2010 Census
#by age group
########################################################


c10a <- cleanCensusData("data/c2010a.csv", sex = "All")
all.equal(subset(pop, Year == 1990)$id, c10a$id)

c10f <- cleanCensusData("data/c2010f.csv", sex = "Females")
all.equal(subset(pop, Year == 1990)$id, c10f$id)

c10m <- cleanCensusData("data/c2010m.csv", sex = "Males")
all.equal(subset(pop, Year == 1990)$id, c10m$id)


pop.census <- makePopEstimates(pop, c10a, "All")
#Check if there are crazy numbers
ddply(pop.census, .(Year), summarise, sum(Population))
pop.census <- melt(pop.census, id = c("id", "MunName", "Year"))
pop.census <- renameVariables(pop.census)
pop.census$Sex <- "Total"
names(pop.census) <- c("id", "MunName", "Year", "AgeGroup", "Population", "Sex")
write.csv(pop.census, "clean-data/pop-census.csv", row.names = FALSE)


popf.census <- makePopEstimates(popwomen, c10f, "Females")
#Check if there are crazy numbers
ddply(popf.census, .(Year), summarise, sum(Population))
popf.census <- melt(popf.census, id = c("id", "MunName", "Year"))
popf.census <- renameVariables(popf.census)
popf.census$Sex <- "Female"
names(popf.census) <- c("id", "MunName", "Year", "AgeGroup", "Population", "Sex")
write.csv(popf.census, "clean-data/popfemale-census.csv", row.names = FALSE)

popm.census <- makePopEstimates(popmen, c10m, "Males")
#Check if there are crazy numbers
ddply(popm.census, .(Year), summarise, sum(Population))
popm.census <- melt(popm.census, id = c("id", "MunName", "Year"))
popm.census <- renameVariables(popm.census)
popm.census$Sex <- "Male"
names(popm.census) <- c("id", "MunName", "Year", "AgeGroup", "Population", "Sex")
write.csv(popm.census, "clean-data/popmale-census.csv", row.names = FALSE)
