# aim - generate expect counts per year (after process-temp)

# y = 2011
# i <- y - 2003 # for testing - comment out for running inside for loop or yrs
temp <- minap[minap$year == y,]


temp$age_band <- cut(temp[, "age"], c(-1, (i+9.99), (i+19.99), (i+26.99), (i+36.99), (i+46.99), (i+56.99), (i+66.99), (i+76.99), (i+86.99), 121),
                     labels=c("0-9", "10-19","20-26","27-36","37-46","47-56","57-66","67-76","77-86", "87+"))


dt <- data.table(temp) # Convert to data table
msoas_age_sex_yr <- dt[, list(admissions=.N), by = c("sex", "age_band", "year", "msoa_code")] # Aggregate up
msoas_age_sex_yr <- as.data.frame(msoas_age_sex_yr)

# Join together population data to temp
msoas_join <- join(msoas_age_sex_yr, as.data.frame(pop_03_13), by = c("age_band", "sex", "year", "msoa_code"), type = "full", match = "all")
msoas_join <- msoas_join[!is.na(msoas_join$population),] # Drop missing population data (i.e. years not required - so gets rid of 2009 and before)

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
