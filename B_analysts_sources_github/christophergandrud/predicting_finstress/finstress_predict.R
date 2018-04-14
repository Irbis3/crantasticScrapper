# ---------------------------------------------------------------------------- #
# Merge FinStress, WDI, OECD, Run basic regressions on FinStress Variance
# Christopher Gandrud
# MIT LICENSE
# ---------------------------------------------------------------------------- #

# Load required packages
library(simpleSetup)

pkgs <- c('rio', 'dplyr', 'lubridate', 'DataCombine',
          'countrycode', 'WDI', 'stargazer', 'tseries')
library_install(pkgs)

# Set working directory
possibles <- c('/git_repositories/predicting_finstress/analysis_data')
set_valid_wd(possibles)

# Load FinStress -------------------------------------------------
FinStress <- rio::import(
    "https://raw.githubusercontent.com/christophergandrud/EIUCrisesMeasure/master/data/FinStress.csv")

# Annual data --------
FinStress$year <- year(FinStress$date)

FinStress$iso2c <- countrycode(FinStress$iso3c, origin = 'iso3c',
                               destination = 'iso2c', warn = TRUE)

finstress <- FinStress %>% select(iso2c, date, year, FinStress) %>%
    rename(finstress = FinStress)

# Annual mean
finstress_yr_mean <- finstress %>% group_by(iso2c, year) %>%
    summarise(finstress_mean = mean(finstress, na.rm = T))

# Annual variance
finstress_yr_var <- finstress %>% group_by(iso2c, year) %>%
    summarise(finstress_var = var(finstress, na.rm = T))

# Annual variance
finstress_yr_sd <- finstress %>% group_by(iso2c, year) %>%
    summarise(finstress_sd = sd(finstress, na.rm = T))

finstress_yr <- merge(finstress_yr_mean, finstress_yr_var,
                      by = c('iso2c', 'year'), all = T)

finstress_yr <- merge(finstress_yr, finstress_yr_sd,
                      by = c('iso2c', 'year'), all = T)

# rescale to make coefficients more easily interpretable
finstress_yr$finstress_var <- finstress_yr$finstress_var * 1000

finstress_yr <- finstress_yr %>% arrange(iso2c, year)

FindDups(finstress_yr, Vars = c('iso2c', 'year'))

# Check stationarity of FinStress Var
for (i in unique(finstress_yr$iso2c)) {
    message(i)
    sub <- subset(finstress_yr, iso2c == i)
    sub <- sub[complete.cases(sub), ]
    print(adf.test(sub$finstress_var))
}

# Lags and leads
finstress_yr <- slide(finstress_yr, Var = 'finstress_var', GroupVar = 'iso2c',
                      NewVar = 'finstress_var_lead1yr', slideBy = 1)
finstress_yr <- slide(finstress_yr, Var = 'finstress_sd', GroupVar = 'iso2c',
                      NewVar = 'finstress_sd_lead1yr', slideBy = 1)

finstress_yr <- slide(finstress_yr, Var = 'finstress_var', GroupVar = 'iso2c',
                      NewVar = 'finstress_var_lag1yr', slideBy = -1)
finstress_yr <- slide(finstress_yr, Var = 'finstress_sd', GroupVar = 'iso2c',
                      NewVar = 'finstress_sd_lag1yr', slideBy = -1)

finstress_yr <- slide(finstress_yr, Var = 'finstress_mean', GroupVar = 'iso2c',
                      NewVar = 'finstress_mean_lead1yr', slideBy = 1)
finstress_yr <- slide(finstress_yr, Var = 'finstress_mean', GroupVar = 'iso2c',
                      NewVar = 'finstress_mean_lag1yr', slideBy = -1)

# Download WDI gdp change & Stock Price Volatility -----------------------------
wdi <- WDI(indicator = c('NY.GDP.MKTP.KD.ZG', 'PA.NUS.FCRF', 'GFDD.SM.01',
                         'GFDD.OM.02'),
           start = 2000, end = 2013, extra = T) %>%
    rename(gdp_growth = NY.GDP.MKTP.KD.ZG) %>%
    rename(exchange_rate_usd = PA.NUS.FCRF) %>%
    rename(stock_price_volatility = GFDD.SM.01) %>%
    rename(stock_returns = GFDD.OM.02)

# Drop poorly coded CV
wdi <- wdi %>% filter(iso2c != 'CV')

# Financial Fragility Indicators from Andrianova et al. (2015) -----------------
ff <- import('raw_data/Financial Fragility Database Stata.dta') %>%
    select(-countryname, -countryid) %>%
    dplyr::rename(iso2c = countrycode)

ff$log_imploans <- log(ff$ImpLoans)


# Laeven and Valencia Banking Crisis Dummy -------------------------------------
lv <- import('https://raw.githubusercontent.com/christophergandrud/EIUCrisesMeasure/master/data/alternative_measures/cleaned/laeven_valencia_banking_crisis.csv')

lv <- slide(lv, Var = 'lv_bank_crisis', GroupVar = 'iso2c',
                      NewVar = 'lv_lead1yr', slideBy = 1)

# Merge ------------------------------------------------------------------------
comb <- merge(finstress_yr, wdi, by = c('iso2c', 'year'), all.x = T)
comb <- merge(comb, ff, by = c('iso2c', 'year'), all.x = T)
comb <- merge(comb, lv, by = c('iso2c', 'year'), all.x = T)

comb <- FindDups(comb, c('iso2c', 'year'), NotDups = TRUE)
comb <- comb %>% filter(!is.na(iso2c))

# Save basic data ---------
export(comb, file = 'combined_data.csv')
comb <- import('combined_data.csv')

comb_high <- comb %>% filter(income == 'High income: OECD')

# Drop non-countries
comb <- subset(comb, iso3c != "")

# Simple regression model ------------------------------------------------------
# Full sample --

## GDP and FinStress
mfull_1 <- lm(finstress_var_lead1yr ~ finstress_var + gdp_growth + iso2c,
               data = comb)
mfull_2 <- lm(finstress_var_lead1yr ~ finstress_var + gdp_growth +
                   finstress_mean + iso2c, data = comb)
mfull_3 <- lm(finstress_var_lead1yr ~ finstress_var + finstress_mean +
                   stock_price_volatility + iso2c, data = comb)
## CAMELS
mfull_4 <- lm(finstress_var_lead1yr ~ finstress_var + log_imploans + iso2c,
               data = comb)

# Using Stand. Dev. instead of Variance
mfull_1_sd <- lm(finstress_sd_lead1yr ~ finstress_sd + gdp_growth + iso2c,
                  data = comb)
mfull_2_sd <- lm(finstress_sd_lead1yr ~ finstress_sd + gdp_growth +
                      finstress_mean + iso2c, data = comb)
mfull_3_sd <- lm(finstress_sd_lead1yr ~ finstress_sd + finstress_mean +
                  stock_price_volatility + iso2c, data = comb)
mfull_4_sd <- lm(finstress_sd_lead1yr ~ finstress_sd + log_imploans + iso2c,
                  data = comb)
mfull_5_sd <- lm(finstress_sd_lead1yr ~ finstress_sd + Liquid + iso2c,
                  data = comb)

# High Income

## GDP and FinStress
moecd_1 <- lm(finstress_var_lead1yr ~ finstress_var + gdp_growth + iso2c,
               data = comb_high)
moecd_2 <- lm(finstress_var_lead1yr ~ finstress_var + gdp_growth  +
                   finstress_mean + iso2c, data = comb_high)
moecd_3 <- lm(finstress_var_lead1yr ~ finstress_var +
                   finstress_mean + stock_price_volatility + iso2c,
               data = comb_high)

## CAMELS
moecd_4 <- lm(finstress_var_lead1yr ~ finstress_var +  log_imploans + iso2c,
               data = comb_high)

# Using Stand. Dev. instead of Variance
mfull_1_sd <- lm(finstress_sd_lead1yr ~ finstress_sd + gdp_growth + iso2c,
                  data = comb)
mfull_2_sd <- lm(finstress_sd_lead1yr ~ finstress_sd + gdp_growth +
                      finstress_mean + iso2c, data = comb)
mfull_3_sd <- lm(finstress_sd_lead1yr ~ finstress_sd + log_imploans + iso2c,
                  data = comb)

moecd_1_sd <- lm(finstress_sd_lead1yr ~ finstress_sd + gdp_growth + iso2c,
                  data = comb_high)
moecd_2_sd <- lm(finstress_sd_lead1yr ~ finstress_sd + gdp_growth  +
                   finstress_mean + iso2c, data = comb_high)

moecd_3_sd <- lm(finstress_sd_lead1yr ~ finstress_sd + log_imploans + iso2c,
               data = comb_high)

## No lagged DV ------------
mfull_nolagdv_1 <- lm(finstress_var_lead1yr ~ gdp_growth + iso2c,
               data = comb)
mfull_nolagdv_2 <- lm(finstress_var_lead1yr ~ gdp_growth +
                   finstress_mean + iso2c, data = comb)
mfull_nolagdv_3 <- lm(finstress_var_lead1yr ~ finstress_mean +
                   stock_price_volatility + iso2c, data = comb)

mfull_nolagdv_4 <- lm(finstress_var_lead1yr ~ log_imploans + iso2c,
               data = comb)

moecd_nolagdv_1 <- lm(finstress_var_lead1yr ~ gdp_growth + iso2c,
             data = comb_high)
moecd_nolagdv_2 <- lm(finstress_var_lead1yr ~ gdp_growth  +
                 finstress_mean + iso2c, data = comb_high)
moecd_nolagdv_3 <- lm(finstress_var_lead1yr ~ finstress_mean +
             stock_price_volatility + iso2c,
             data = comb_high)
moecd_nolagdv_4 <- lm(finstress_var_lead1yr ~ log_imploans + iso2c,
           data = comb_high)


## Annual For Paper ------
stargazer(mfull_nolagdv_1, mfull_nolagdv_2, mfull_nolagdv_3, mfull_nolagdv_4,
          moecd_nolagdv_1, moecd_nolagdv_2, moecd_nolagdv_3, moecd_nolagdv_4,
          omit = 'iso2c*',
          type = 'latex',
          dep.var.labels = 'Var(FinStress)$_{year+1}$',
          covariate.labels = c('GDP Growth (\\%)', 
                               'FinStress Mean$_{year}$',
                               'Stock Price Volatility',
                               'Impaired Loans (log)') ,
          column.labels = c(rep('Full Sample', 4), rep('OECD', 4)),
          add.lines = list(c('Country FE', rep('y', 8))),
          label = 'annual_reg',
          title = 'Regression result from predicting FinStress Variance using annual explanatory variable data',
          out = 'results_tables/annual_regressions.tex',
          font.size = 'tiny',
          omit.stat = 'f'
          )

## Annual No Lagged DV For Paper ------
stargazer(mfull_nolagdv_1, mfull_nolagdv_2, mfull_nolagdv_3, mfull_nolagdv_4,
          moecd_nolagdv_1, moecd_nolagdv_2, moecd_nolagdv_3, moecd_nolagdv_4,
          omit = 'iso2c*',
          type = 'latex',
          dep.var.labels = 'Var(FinStress)$_{year+1}$',
          covariate.labels = c('GDP Growth (\\%)', 'FinStress Mean$_{year}$',
                               'Stock Price Volatility',
                               'Impaired Loans (log)') ,
          column.labels = c(rep('Full Sample', 4), rep('OECD', 4)),
          add.lines = list(c('Fixed Effects', rep('y', 8))),
          label = 'annual_reg_nolag_dv',
          title = 'Regression result from predicting FinStress Variance using annual explanatory variable data (without lagged dependent variable)',
          out = 'results_tables/annual_noDVlag_regressions.tex',
          font.size = 'tiny',
          omit.stat = 'f'
)

## Annual For Presentation ------
stargazer(mfull_1, mfull_2, mfull_3, mfull_4,
          omit = 'iso2c*',
          type = 'latex',
          dep.var.labels = 'Var(FinStress)$_{year+1}$',
          covariate.labels = c('Var(FinStress)$_{year+0}$',
                               'GDP Growth (\\%)', 'FinStress Mean$_{year}$',
                               'Stock Price Volatility',
                               'Impaired Loans (log)') ,
          column.labels = c(rep('Full Sample', 4)),
          add.lines = list(c('Fixed Effects', rep('y', 4))),
          label = 'annual_reg',
          title = 'Regression result from predicting FinStress Variance using annual explanatory variable data (Full Sample)',
          out = 'results_tables/annual_regressions_all.tex',
          font.size = 'tiny',
          omit.stat = 'f'
)

## Annual For Paper ------
stargazer(moecd_1, moecd_2, moecd_3, moecd_4,
          omit = 'iso2c*',
          type = 'latex',
          dep.var.labels = 'Var(FinStress)$_{year+1}$',
          covariate.labels = c('GDP Growth (\\%)',
                               'FinStress Mean$_{year}$',
                               'Stock Price Volatility',
                               'Impaired Loans (log)',
                               'Liquid Assets Ratio') ,
          column.labels = rep('OECD', 4),
          add.lines = list(c('Fixed Effects', rep('y', 4))),
          label = 'annual_reg',
          title = 'Regression result from predicting FinStress Variance using annual explanatory variable data (OECD Sample)',
          out = 'results_tables/annual_regressions_oecd.tex',
          font.size = 'tiny',
          omit.stat = 'f'
)

#------------------------------------------------------------------------------#
# Quarterly data ----------------------
finstress <- FinStress %>% select(iso2c, date, FinStress) %>%
    rename(finstress = FinStress) %>% rename(quarter = date)

finstress$quarter <- quarter(finstress$quarter, with_year = T)

# Quarterly mean
finstress_qt_mean <- finstress %>% group_by(iso2c, quarter) %>%
    summarise(finstress_mean = mean(finstress, na.rm = T))

# Quarterly variance
finstress_qt <- finstress %>% group_by(iso2c, quarter) %>%
    summarise(finstress_var = var(finstress, na.rm = T))

finstress_qt <- merge(finstress_qt_mean, finstress_qt,
                      by = c('iso2c', 'quarter'), all = T)

# rescale to make coefficients more easily interpretable
finstress_qt$finstress_var <- finstress_qt$finstress_var * 1000

finstress_qt <- finstress_qt %>% arrange(iso2c, quarter)

FindDups(finstress_qt, Vars = c('iso2c', 'quarter'))

finstress_qt <- slide(finstress_qt, Var = 'finstress_var', GroupVar = 'iso2c',
                      NewVar = 'finstress_var_lead1qt', slideBy = 1)

finstress_qt <- slide(finstress_qt, Var = 'finstress_var', GroupVar = 'iso2c',
                      NewVar = 'finstress_var_lag1qt', slideBy = -1)

# Load quarterly gdp growth (seasonally adjusted): Originally downloaded from:
# https://stats.oecd.org
oecd <- import('raw_data/QNA_06112015123914289.csv')
oecd <- oecd %>%
    filter(Measure == "Growth rate compared to the same quarter of previous year, seasonally adjusted" &
               Subject == 'Gross domestic product - expenditure approach')
oecd <- oecd[, c(2, 9, 17)]
names(oecd) <- c('country', 'quarter', 'gdp_growth_oecd')

oecd$quarter <- gsub('-Q', '\\.', oecd$quarter)
oecd$iso2c <- countrycode(oecd$country, origin = 'country.name',
                          destination = 'iso2c')
oecd <- oecd %>% select(-country)

FindDups(oecd, c('iso2c', 'quarter'))

# Merge together
comb_qt <- merge(oecd, finstress_qt, by = c('iso2c', 'quarter'))
comb_qt <- DropNA(comb_qt, 'iso2c') # NA is Euro area

# Quarterly regressions -------
mqt_1 <- lm(finstress_var_lead1qt ~ gdp_growth_oecd + iso2c,
             data = comb_qt)

mqt_2 <- lm(finstress_var_lead1qt ~ gdp_growth_oecd +
                 finstress_mean + iso2c,
             data = comb_qt)


stargazer(mqt_1, mqt_2, type = 'latex',
          omit = 'iso2c*',
          dep.var.labels = 'Var(FinStress)$_{quarter+1}$',
          covariate.labels = c('GDP Growth (\\%)',
                                'FinStress Mean$_{quarter + 0}$'),
          add.lines = list(c('Country FE', 'y', 'y')),
          label = 'quarterly_reg',
          title = 'Regression result from predicting FinStress Variance using quarterly explanatory variable data (OECD only)',
          out = 'results_tables/quarterly_regressions.tex',
          font.size = 'small',
          omit.stat = 'f'
          )
