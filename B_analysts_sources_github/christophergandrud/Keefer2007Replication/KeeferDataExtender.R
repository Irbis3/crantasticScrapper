################
# Keefer data extender
# Christopher Gandrud
# 17 March 2015
###############

library(DataCombine)
library(countrycode)
library(psData)
library(forecast)
library(dplyr)
library(repmis)
library(WDI)
library(foreign)
library(repmis)

# Set working directory. Change as needed.
setwd('~/git_repositories/Keefer2007Replication/')

# Fuction for keefer rolling 3 year averages
rollmean3r <- function(x){
    x <- shift(x, -2)
    ma(x, 3, centre = FALSE)
}

rollmean3f <- function(x){
    x <- shift(x, 2)
    ma(x, 3, centre = FALSE)
}

rollmean33 <- function(x){
    xR <- rollmean3r(x)
    xF <- rollmean3f(x)
    Comb <- (xR + xF)/2
}

#### Fiscal transfers data (both Laeven and Valencia (2012) and Keefer (2007))
Fiscal <- read.csv('data/KeeferFiscal.csv', stringsAsFactors = F)
Fiscal <- VarDrop(Fiscal, 'country')
Fiscal$HonohanCrisisOngoing[is.na(Fiscal$HonohanCrisisOngoing)] <- 0

Fiscal <- Fiscal %>% select(-Notes)

Fiscal <- subset(Fiscal, iso2c != '') # Drop Czechoslovakia

#### Database of Political Institutions data 
dpiVars <- c('eiec', 'checks', 'stabns')
DpiData <- DpiGet(vars = dpiVars, OutCountryID = 'iso2c', duplicates = 'drop')
DpiData[, dpiVars][DpiData[, dpiVars] == -999] <- NA

DpiData <- DpiData[order(DpiData$country, DpiData$year), ]

# Dichotomize electoral competitiveness
DpiData$DiEiec <- 0
DpiData$DiEiec[DpiData$eiec >= 6] <- 1

# Create Keefer Forward and Backward Lags
DpiData <- DpiData %>% group_by(country) %>% 
            mutate(DiEiec33 = rollmean33(DiEiec))
DpiData <- DpiData %>% group_by(country) %>% 
            mutate(Checks33 = rollmean33(checks))

# Create backwards lags
DpiData <- DpiData %>% group_by(country) %>%
            mutate(stabnsLag3 = rollmean3r(stabns))

# Find residuals for lagged check
Sub <- DropNA(DpiData, c('DiEiec33', 'Checks33'))
Resid <- lm(DiEiec33 ~ Checks33, data = Sub)
Sub$ChecksResiduals33 <- Resid$residuals
Sub <- Sub[, c('iso2c', 'year', 'ChecksResiduals33')]

# Euro indicator
eu <- 'http://bit.ly/1yRvycq' %>%
        source_data(format = 'csv') %>%
        select(iso2c, year)
eu$eurozone <- 1

# High income by world bank definition (>= 12,746)
gdp <- WDI(indicator = 'NY.GDP.PCAP.CD', start = 1960, end = 2013) %>%
        select(iso2c, year, NY.GDP.PCAP.CD)

gdp$high_income <- 0
gdp$high_income[gdp$NY.GDP.PCAP.CD >= 12746] <- 1
gdp <- gdp %>% select(-NY.GDP.PCAP.CD)

##### Combine data sets
Comb <- dMerge(DpiData, Sub, Var = c('iso2c', 'year'), all.x = TRUE)
Comb <- dMerge(Comb, Fiscal, Var = c('iso2c', 'year'), all.y = TRUE)
Comb <- dMerge(Comb, eu, Var = c('iso2c', 'year'), all.x = TRUE)
Comb <- dMerge(Comb, gdp, Var = c('iso2c', 'year'), all.x = TRUE)
Comb$country <- countrycode(Comb$iso2c, origin = 'iso2c',
                            destination = 'country.name')
Comb$eurozone[is.na(Comb$eurozone)] <- 0

# Create EU dummy
Comb$eu <- Comb$eurozone
Comb$eu[Comb$country == 'France' & Comb$year == 1994] <- 1
Comb$eu[Comb$country == 'Denmark' & Comb$year == 2008] <- 1
Comb$eu[Comb$country == 'Hungary' & Comb$year == 2008] <- 1
Comb$eu[Comb$country == 'Sweden' & Comb$year == 2008] <- 1
Comb$eu[Comb$country == 'United Kingdom' & Comb$year == 2007] <- 1


Comb <- Comb %>% arrange(country, year)

write.dta(Comb, file = 'data/KeeferExtended_RandP.dta')


#### Find proportion of countries that are high income ####
hk <- DropNA(Comb, "Honohan2003.Fiscal")
sum(hk$high_income) / nrow(hk)

lv <- DropNA(Comb, "LV2012.Fiscal")
sum(lv$high_income) / nrow(lv)
