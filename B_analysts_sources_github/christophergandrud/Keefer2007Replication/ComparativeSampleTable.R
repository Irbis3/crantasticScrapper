###########
# Replication file comparative sample table in 'When All is Said and Done'
# Christopher Gandrud
# 17 March 2015
###########

# Set data directory
DD <- '~/git_repositories/Keefer2007Replication/data/'

# Set table directory
TD <- '~/git_repositories/Keefer2007Replication/tables/'

# Load packages
library(foreign)
library(DataCombine)
library(countrycode)
library(dplyr)
library(xtable)

# -------------------------------------------------------------------- #
#### Compare fiscal costs in LV vs. HK ####
## Data set created using:
## https://github.com/christophergandrud/CrisisDataIssues/blob/master/source/DataCreators/KeeferDataExtender.R

Main <- read.dta(paste0(DD, 'KeeferExtended_RandP.dta'))

#### Subset each sample ####
# All independent variables not NA
sub <- Main %>% DropNA(c('ChecksResiduals33', 'DiEiec33', 'stabnsLag3'))

sub <- sub %>% arrange(country, year)

# Shorten country names
sub$country <- gsub(', Province of China', '', sub$country)
sub$country <- gsub(', Bolivarian Republic of', '', sub$country)
sub$country <- gsub(', United Republic of', '', sub$country)
sub$country <- gsub(', Plurinational State of', '', sub$country)
sub$country <- gsub(', United Republic of', '', sub$country)
sub$country <- gsub('Russian Federation', 'Russia', sub$country)
sub$country <- gsub('Dominican Rep.', '', sub$country)

# Crisis year
datayears <- function(df, x) {
    df$temp[!is.na(df[, x])] <- 1
    df[, 'year'] * df[, 'temp']
}

# Function to create colour values
colouriser <- function(x) abs((x/2) - 1)

# Function to table columns rows
paster <- function(df, x) paste0(df[, 'country'], ' ', df[, x],
                             '\\cellcolor[gray]{', df[, 'colour'], '}')

sub$hk_year <- datayears(sub, 'Honohan2003_Fiscal') 
sub$keefer_year <- datayears(sub, 'Keefer2007_Fiscal')
sub$lv_keefer_year <- datayears(sub, 'LV2012_Fiscal') 
    sub$lv_keefer_year[is.na(sub$Keefer2007_Fiscal)] <- NA
sub$lv_pre2001_year <- datayears(sub, 'LV2012_Fiscal') 
    sub$lv_pre2001_year[sub$year >= 2001] <- NA
sub$lv_full_year <- datayears(sub, 'LV2012_Fiscal') 

sub <- sub %>% select(country, contains('_year'), DiEiec33)

# Remove observations with no observed crisis costs
sub$temp <- rowSums(sub[, 2:6], na.rm = T)
sub <- sub %>% filter(temp != 0) %>% select(-temp)

# Create colour scale values
sub$colour <- colouriser(sub$DiEiec33)

for (i in names(sub)[2:6]) {
    sub[, paste0(i, '_comb')] <- paster(sub, i)
    sub[, paste0(i, '_comb')][grep(' NA', sub[, paste0(i, '_comb')])] <- NA
}

sub <- sub %>% select(contains('_comb'))

clean_names <- c('HK', 'Keefer' , 'LV-Keefer', 'LV pre-2001', 'LV Full')
clean_names <- paste0('\\textbf{', clean_names, '}')

names(sub) <- clean_names

# Print table
options(xtable.sanitize.text.function=identity)
print.xtable(xtable(sub, 
             caption = 'Country-Crisis Samples Used in the Regression Models (darker shading = more electorally competitive using the Keefer measure)',
             label = 'samplesTable'),
             caption.placement = 'top',
             include.rownames = F, size = 'tiny',
             comment = F,
             file = paste0(TD, 'RegressionSamples.tex'))
