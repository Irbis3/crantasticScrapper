###########################
# Run stringency model in parallel
# Christopher Gandrud
# MIT License
# Stan version used: 2.9.0
###########################

## Set up -----------------------------------------
# Load packages
library(rio); library(repmis)
library(DataCombine)
library(tidyr)
library(dplyr)
library(devtools)
library(rstan)
library(parallel)

# Set working directory
possibles <- c('/git_repositories//macropru/')

set_valid_wd(possibles)

## Set out width
options('width' = 200)

## Load data and subset -----------------------
Base <- import('data/raw/Data_ReinhardtSowerbutts 2015 BoE WP 546.dta')

# Create ID numbers
Base$countrynum <- as.numeric(as.factor(Base$countrycode))
Base$quarter_year <- sprintf('%s.%s', Base$year, Base$quarter)
Base$quarternum <- as.numeric(as.factor(Base$quarter_year))

# Find items
tighten <- names(Base)[grep("*_Tighten", names(Base))]
loosen <- names(Base)[grep("*_Loosen", names(Base))]
# binary_vars <- c(tighten, loosen)
binary_vars <- c(tighten)


BaseStanVars <- Base[, c('countrynum', 'quarternum', binary_vars)]

# Data descriptions
NCountry <- max(BaseStanVars$countrynum)
NYear_Quarter <- max(BaseStanVars$quarternum)
NItems <- length(binary_vars)

# Melt data so that it is easy to enter into Stan data list
MoltenBase <- gather(BaseStanVars, variable, value, 3:ncol(BaseStanVars))

# Convert item names to numeric
MoltenBase$variable <- as.factor(MoltenBase$variable) %>% as.numeric()

# Order data
MoltenReady <- arrange(MoltenBase, countrynum, quarternum, variable)

#### Specify Model ------------------------------

#### Create data list for Stan ####
macro_data <- list(
    C = NCountry,
    T = NYear_Quarter,
    K = NItems,
    N = nrow(MoltenReady),
    cc = MoltenReady$countrynum,
    tt = MoltenReady$quarternum,
    kk = MoltenReady$variable,
    y = MoltenReady$value
)

# Create Empty Stan model (so it only needs to compile once)
empty_stan <- stan(file = 'analysis/macro_pru_stringency/macro_string.stan', 
                   data = macro_data, chains = 0)

# Run on 4 cores
sflist <-
    mclapply(1:4, mc.cores = 4,
             function(i) stan(fit = empty_stan, data = macro_data,
                              seed = i, chains = 1, thin = 25,
                              iter = 100, chain_id = i,
                              pars = c('delta', 'alpha', 'beta', 'gamma')
             )
    )

# Collect in to Stan fit object
fit <- sflist2stanfit(sflist)
