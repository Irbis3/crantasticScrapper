#################
# Correct DPI yrcurnt years to elections variable to reflect government, not
# executive elections (Europe + OECD)
# Christopher Gandrud
# 22 November 2016
# MIT License
#################

# Load packages
library(psData)
library(DataCombine)
library(countrycode)
library(dplyr)
library(rio)

# Set working directory, change as needed.
setwd('/git_repositories/yrcurnt_corrected/')

# Years left in current term
YearsLeft <- DpiGet(vars = c('yrcurnt'), duplicates = 'drop', fromLast = TRUE)
# YearsLeft$iso2c[YearsLeft$iso2c == 'GB'] <- 'UK'

YearsLeft$country <- countrycode(YearsLeft$iso2c, origin = 'iso2c', 
                                 destination = 'country.name') 

YearsLeft <- YearsLeft %>% select(iso2c, country, year, yrcurnt)

# Keep only European 28 + select OECD
keep <- c("Australia", "Austria", "Belgium", "Bulgaria", "Canada", 
          "Croatia", "Cyprus", "Czech Republic", "Denmark", "Estonia", 
          "Finland", "France", "Germany", "Greece", "Hungary", 
          "Iceland", "Ireland", "Israel", "Italy", "Japan",
          "Korea, Republic of", "Latvia", "Lithuania", "Luxembourg", "Malta", 
          "Mexico", "Netherlands", "New Zealand", "Norway", "Poland", 
          "Portugal", "Romania", "Slovakia", "Slovenia", "Spain", "Sweden", 
          "Switzerland", "United Kingdom", "United States")

YearsLeft <- YearsLeft[YearsLeft$country %in% keep, ]
YearsLeft <- YearsLeft[order(YearsLeft$country, YearsLeft$year), ]

# Recode -999 as NA
YearsLeft$yrcurnt[YearsLeft$yrcurnt == -999] <- NA

#### Create corrected time to election variable ####
# YearsLeft$yrcurnt_gov <- YearsLeft$yrcurnt

## Recode focusing on parliamentary elections, if executive is a figurehead
## Recode all election years as 0
## From the European Election Database (http://www.nsd.uib.no/european_election_database)
## and Wikipedia

## See README for summary of changes.

# Merge in changes
updated <- import('data/yrcurnt_corrected.csv')
comb <- merge(YearsLeft, updated, by = c('country', 'year'), all = TRUE)

# Create complete merged/corrected yrcurnt
for (i in 1:nrow(comb)) {
    if (is.na(comb[i, 'yrcurnt_corrected'])) {
        comb[i, 'yrcurnt_corrected'] <- comb[i, 'yrcurnt']
    }
}

# Final clean
comb <- MoveFront(comb, c('worldbank_code', 'iso2c'))
comb$worldbank_code <- countrycode(comb$country, origin = 'country.name',
                                   destination = 'wb')
comb$iso2c <- countrycode(comb$country, origin = 'country.name',
                                   destination = 'iso2c')

comb <- DropNA(comb, 'yrcurnt_corrected')


######### Drop post-2013 -- These have not been checked -- To-do ###############
comb <- subset(comb, year <= 2013)

# Save
export(comb, 'data/yrcurnt_original_corrected.csv')
