#################################################
###### Myocardial Infarction Cycling Paper ######
### run after process-pop & export-minap-labs ###
#################################################

# Libraries
library(plyr)
library(dplyr)
library(sp)
library(tmap)
library(data.table)

# Load data
# sample_data = readr::read_csv("data/testdat.csv")
# sample_data <- readRDS("data/minap-sample.Rds")
# source("R/export-minap-labs.R") # if you're working with full dataset on secure computer
minap <- readRDS("data/sample_data.Rds") # For Mark
vars <- c("year", "age", "sex", "easting", "northing") # Will need to add more later
minap = minap[vars]

# Keep only years interested in [need to make a decision on this - I have picked 2010-2013 for now]
# minap <- minap[minap$year > 2009,]
# Remove observations with no location
minap = minap[!is.na(minap$easting) & !is.na(minap$easting),]

## Attach MSOA codes here ##
coords = cbind(minap$easting * 100, minap$northing * 100)
minap_sp = SpatialPointsDataFrame(coords = coords, data = minap)
rm(coords)

bbox(minap_sp)
proj4string(minap_sp) = CRS("+init=epsg:27700")
minap_sp = spTransform(x = minap_sp, CRSobj = CRS("+init=epsg:4326"))
bbox(minap_sp)

las = readRDS("data/las-geo-mode.Rds")
names(las)
# RL: add definitive code from raw data to generate mode split (by age for cycling and walking all)
msoas = readRDS("data/msoas.Rds")
names(msoas)
# plot(las, lwd = 3)
# plot(msoas, add = T) # just english msoas for now
# plot(minap_sp, col = "red", add = T)

# # Add age-specific msoa data # commented out
# msoas_age_mode = geojsonio::geojson_read("data/msoas-age-mode.geojson", what = "sp")
# msoas_age_mode@data[8:ncol(msoas_age_mode)] = apply(msoas_age_mode@data[8:ncol(msoas_age_mode)], 2, as.numeric)
# names(msoas_age_mode)
# mjoined = inner_join(msoas@data, msoas_age_mode@data[c(1, 8:ncol(msoas_age_mode))], by = "geo_code")
# head(msoas@data)
# head(mjoined)
# msoas@data = mjoined
# Add minap counts to msoas
msoas$count = aggregate(minap_sp["year"], msoas, FUN = length)$year # convert to msoa level
sum(msoas$count, na.rm = T) # the 700,000 cases counted by msoa code
plot(msoas$All, msoas$count)
cor(msoas$All, msoas$count, use = "complete.obs")

# m = tm_shape(msoas) +
#   tm_fill("count", style = "quantile")
# save_tmap(m, "figures/counts.png")

# Add msoa level data to minap data
o = over(minap_sp, msoas)
minap = cbind(minap, o[c("geo_code", "All", "Car", "Bicycle", "foot")])
# # Save MSOA cycling data seperately - not sure what this is doing...
# library(dplyr)
# library(dtplyr)
# hold <- minap[c("geo_code", "All", "Car", "Bicycle", "foot")] # Subset
# transport_msoa <- hold %>% distinct(geo_code) # Drop duplicate MSOAs
# saveRDS(transport_msoa, "data/msoas_transport_data.Rds")
# rm(transport_msoa)
# rm(hold)
# Create age bands

minap$age_band <- cut(minap[, "age"], c(-1, 9.99, 17.5, 24.5, 34.5, 44.5, 54.5, 64.5, 74.5, 86.99, 121),
                      labels=c("0-9", "10-19","20-26","27-36","37-46","47-56","57-66","67-76","77-86", "87+")) # for 2011
head(minap)


# Aggregate counts to MSOAs
minap$msoa_code <- minap$geo_code # Rename variable
dt <- data.table(minap) # Convert to data table
msoas_age_sex_yr <- dt[, list(admissions=.N), by = c("sex", "age_band", "year", "msoa_code")] # Aggregate up
msoas_age_sex_yr <- as.data.frame(msoas_age_sex_yr)
# rm(minap)
# rm(dt)

## Join on population data in same format here based on MSOA data
# Load population data
load("data/pop_03_13.RData")

# Join together population data to MINAP
msoas_join <- join(msoas_age_sex_yr, as.data.frame(pop_03_13), by = c("age_band", "sex", "year", "msoa_code"), type = "full", match = "all")
# msoas_join <- left_join(msoas_age_sex_yr, pop_03_13)
msoas_join <- msoas_join[!is.na(msoas_join$population),] # Drop missing population data (i.e. years not required - so gets rid of 2009 and before)
#rm(msoas_age_sex_yr)
#rm(pop_02_13)

### Create expected counts ###
# Aggregate counts by age and sex to calcuate the 'standard population'
hold <- data.table(msoas_join)
std_pop <- hold[, list(admissions = sum(admissions, na.rm = TRUE), population = sum(population, na.rm = TRUE)),
                by = c("sex", "age_band", "year")]
#rm(hold)

# Calculate age- and sex-specific rates
std_pop <- as.data.frame(std_pop)
std_pop$adm_rate <- std_pop$admissions / std_pop$population

std_pop <- std_pop[is.finite(std_pop$adm_rate),] # Get rid of missing data
std_pop

std_pop$population <- NULL # Delete unnceessary variables
std_pop$admissions <- NULL

# Join the age- and sex-specific rates onto the data
msoa_exp_obs <- join(msoas_join, std_pop, by = c("sex", "age_band", "year"), type = "left", match = "all")
#rm(msoas_join)
#rm(std_pop)

# Calcuate expected rate
msoa_exp_obs$expt_adms <- msoa_exp_obs$adm_rate * msoa_exp_obs$population
msoa_exp_obs$expt_adms[is.na(msoa_exp_obs$expt_adms)] <- 0
msoa_exp_obs$adm_rate <- NULL

# Check adds up
sum(msoa_exp_obs$admissions, na.rm=T)
sum(msoa_exp_obs$expt_adms, na.rm=T)


# Save data
msoa_exp_obs$admissions[is.na(msoa_exp_obs$admissions)] <- 0
saveRDS(msoa_exp_obs, "data/msoas_observed_expected_counts.Rds")
summary(msoa_exp_obs)


## What are left with is a file for MSOAs disaggregated by sex and age-bands (by year) with counts of
## admissions, population and the expected count of admissions. We can later aggregate by sex (or for total
## persons) the counts but better to keep disaggregated for now


### Do the same for Local Authority Level (District/UA) ###

lkup <- readr::read_csv("data/la_msoa_lkup.csv") # Load LA to MSOA lookup
la_data <- join(msoa_exp_obs, lkup, by = "msoa_code", type = "right", match = "all") # Join together

dt <- data.table(la_data) # Convert to data table
la_age_sex_yr <- dt[, list(admissions=sum(admissions, na.rm = TRUE), population=sum(population, na.rm = TRUE)),
                    by = c("sex", "age_band", "year", "la_code")] # Aggregate by LA, age and sex

# Create expected counts #

# Aggregate counts by age and sex to calcuate the 'standard population'
hold <- data.table(la_age_sex_yr)
std_pop <- hold[, list(admissions = sum(admissions, na.rm = TRUE), population = sum(population, na.rm = TRUE)),
                by = c("sex", "age_band", "year")]
rm(hold)

# Calculate age- and sex-specific rates
std_pop <- as.data.frame(std_pop)
std_pop$adm_rate <- std_pop$admissions / std_pop$population

std_pop <- std_pop[is.finite(std_pop$adm_rate),] # Get rid of missing data
std_pop

std_pop$population <- NULL # Delete unnceessary variables
std_pop$admissions <- NULL

# Join the age- and sex-specific rates onto the data
la_exp_obs <- join(la_age_sex_yr, std_pop, by = c("sex", "age_band", "year"), type = "left", match = "all")
rm(la_age_sex_yr)
rm(std_pop)

# Calcuate expected rate
la_exp_obs$expt_adms <- la_exp_obs$adm_rate * la_exp_obs$population
la_exp_obs$expt_adms[is.na(la_exp_obs$expt_adms)] <- 0
la_exp_obs$adm_rate <- NULL

sum(la_exp_obs$admissions, na.rm=T)
sum(la_exp_obs$expt_adms, na.rm=T)
summary(la_exp_obs)

# Save data
saveRDS(la_exp_obs, "data/las_observed_expected_counts.Rds")
# rm(msoa_exp_obs)

gc()

# next for LA analysis - R/model-yearly.R
