# -------------------
# Basic exploration of EU member states' statistical agency directors
# Christopher Gandrud
# MIT LICENSE
# -------------------

# Set working directory. Change as needed.
setwd('/git_repositories/eu_ms_statistical_agency_tenure/')

# Load required packages
library(rio)
library(dplyr)
library(countrycode)
library(DataCombine)
library(lubridate)
library(ggplot2)
library(tidyr)
theme_set(theme_bw())

# Load director tenure data (gathered by Laurence Hendry)
tenure <- import('raw_data/Statistical Agencies Spreadsheet.xlsx', 
                 which = 'Directors lists')

tenure$iso3c <- countrycode(tenure$Country, origin = 'country.name',
                            destination = 'iso3c')
tenure <- subset(tenure, !is.na(Country))


# Country-year-director name
tenure_sub <- tenure[, c('iso3c', 'Year', 'Name')]
names(tenure_sub) <- c('iso3c', 'year', 'director_name')

# Find spells and change years 
tenure_sub <- tenure_sub %>% group_by(iso3c) %>%
    mutate(director_lag = dplyr::lag(director_name, n = 1))

# Find if there is a change
tenure_sub$director_change <- 0
tenure_sub$director_change[tenure_sub$director_name != tenure_sub$director_lag] <- 1
tenure_sub$director_change[is.na(tenure_sub$director_lag)] <- NA

# Download election timing ---------------------
URL <- 'https://raw.githubusercontent.com/christophergandrud/yrcurnt_corrected/master/data/yrcurnt_corrected.csv'

election_timing <- import(URL)
election_timing$iso3c <- countrycode(election_timing$country, 
                                     origin = 'country.name', 
                                     destination = 'iso3c')
election_timing <- select(election_timing, iso3c, year, yrcurnt_corrected)


# Cabinet changes -------------
# Data from Data http://www.parlgov.org/
cabinet <- import('raw_data/view_cabinet.csv')

cabinet$iso3c <- countrycode(cabinet$country_name, 
                             origin = 'country.name', 
                             destination = 'iso3c')

cabinet <- FindDups(cabinet, Vars = c('cabinet_name'), NotDups = TRUE)
cabinet$year <- year(cabinet$start_date)
cabinet$cabinet_change <- 1

cabinet <- select(cabinet, iso3c, year, cabinet_change)

# Merge -------------
comb <- merge(tenure_sub, cabinet, by = c('iso3c', 'year'), 
              all.x = TRUE)

# cabinet_change = 0 for years without cabinet change information, 
# NA for 2016
comb$cabinet_change[is.na(comb$cabinet_change)] <- 0
comb$cabinet_change[comb$year == 2016] <- NA

comb$director_change <- factor(comb$director_change, 
                               labels = c('No Change', 'Change'))
comb$cabinet_change <- factor(comb$cabinet_change, 
                              labels = c('No Change', 'Change'))

# Plot series
comb_tidy <- select(comb, iso3c, year, director_change, cabinet_change)
comb_tidy <- gather(comb_tidy, change_type, value, 3:4)

comb_tidy <- subset(comb_tidy, !is.na(value))

ggplot(comb_tidy, aes(year, value, group = change_type, colour = change_type)) +
    facet_wrap(~iso3c) + 
    geom_line(alpha = 0.5)



# Overall relationship
dir_cab_table <- table(comb$cabinet_change, comb$director_change)
prop.table(dir_cab_table, margin = 1)
chisq.test(dir_cab_table)


