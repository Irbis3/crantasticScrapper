# ---------------------------------------------------------------------------- #
# Elections, Electoral Loss Probabilities, and Government Fiscal Policies
# Version 2
# Christopher Gandrud
# MIT License
# ---------------------------------------------------------------------------- #

library(rio)
library(dplyr)
library(lubridate)
library(plm)
library(DataCombine)
library(ggplot2)

# Set working directory. Change as needed.
setwd('/git_repositories/financial_crisis_fiscal_policy/')

comb <- import('analysis_data/covariate_data/epfms_covariates.csv')

# Convert key varaiables to factors
comb$iso2c <- comb$iso2c %>% as.factor
comb$election_year <- comb$election_year %>% as.factor
comb$election_year_1 <- comb$election_year_1 %>% as.factor

##### Create residuals #####
# Output Gap Residuals
m_r1 <- lm(cent_debt_gdp2005 ~ cent_debt_gdp2005_1 + output_gap + 
                    iso2c, data = comb)

sub_debt <- comb %>% DropNA(c('cent_debt_gdp2005', 'cent_debt_gdp2005_1', 
                                           'output_gap'))
sub_debt$residuals_output_debt <- residuals(m_r1)
sub_debt <- slide(sub_debt, Var = 'residuals_output_debt', 
                               NewVar = 'residuals_output_debt_1',
                               GroupVar = 'country', TimeVar = 'year')

# Financial Stress Residuals
m_r2_basic <- lm(residuals_output_debt ~ residuals_output_debt_1 + 
               lv_bank_crisis, 
           data = sub_debt)

m_r2 <- lm(residuals_output_debt ~ residuals_output_debt_1 + 
               lv_bank_crisis*mean_stress, 
           data = sub_debt)

sub_debt <- sub_debt %>% DropNA(c('residuals_output_debt_1', 'lv_bank_crisis',
                                  'mean_stress'))
sub_debt$residuals_stress_debt <- residuals(m_r2)

sub_debt <- slide(sub_debt, Var = 'residuals_stress_debt', 
                               NewVar = 'residuals_stress_debt_1',
                               GroupVar = 'country', TimeVar = 'year')

sub_debt$rs_change_debt <- sub_debt$residuals_stress_debt -
    sub_debt$residuals_stress_debt_1 

sub_debt <- slide(sub_debt, Var = 'rs_change_debt', 
                               NewVar = 'rs_change_debt_1',
                               GroupVar = 'country', TimeVar = 'year')

# ------------------------- Econ Spending Residuals -------------- #
#### Create Total Spending Residuals ####
m_r1_econ <- lm(gov_econ_spend_gdp2005 ~ gov_econ_spend_gdp2005_1 + output_gap + 
                    iso2c,
                data = comb)

sub_gov_spend <- comb %>% DropNA(c('gov_econ_spend_gdp2005', 
                                        'gov_econ_spend_gdp2005_1',
                                        'output_gap'))
sub_gov_spend$residuals_output_spend <- residuals(m_r1_econ)
sub_gov_spend <- slide(sub_gov_spend, Var = 'residuals_output_spend', 
                      NewVar = 'residuals_output_spend_1',
                      GroupVar = 'country', TimeVar = 'year')

m_r2_econ_basic <- lm(residuals_output_spend ~ residuals_output_spend_1 + 
                    lv_bank_crisis, data = sub_gov_spend)

m_r2_econ <- lm(residuals_output_spend ~ residuals_output_spend_1 + 
                    lv_bank_crisis*mean_stress, data = sub_gov_spend)
sub_gov_spend <- sub_gov_spend %>% 
                        DropNA(c('residuals_output_spend_1', 'lv_bank_crisis',
                                 'mean_stress'))
sub_gov_spend$residuals_stress_spend <- residuals(m_r2_econ)

sub_gov_spend <- slide(sub_gov_spend, Var = 'residuals_stress_spend', 
                      NewVar = 'residuals_stress_spend_1',
                      GroupVar = 'country', TimeVar = 'year')

sub_gov_spend$rs_change_spend <- sub_gov_spend$residuals_stress_spend -
                                    sub_gov_spend$residuals_stress_spend_1 

sub_gov_spend <- slide(sub_gov_spend, Var = 'rs_change_spend', 
                      NewVar = 'rs_change_spend_1',
                      GroupVar = 'country', TimeVar = 'year')

# Merge change in off-trend spending to off-trend debt
to_merge <- sub_gov_spend %>% select(iso2c, year, rs_change_spend, 
                                     rs_change_spend_1)

sub_debt <- merge(sub_debt, to_merge, by = c('iso2c', 'year'), 
                      all.x = T)

# ------------------------------- Regressions -------------------------------- #
#### Set up plm object ####
# Not done before due to issues with subsetting data set for residuals
sub_gov_spend <- pdata.frame(sub_gov_spend, index = c('iso2c', 'year'))
sub_debt <- pdata.frame(sub_debt, index = c('iso2c', 'year'))

#### Election Year #### 
# Spending
m1_t0 <- lm(rs_change_spend ~ rs_change_spend_1 + election_year + lpr_1, 
            data = sub_gov_spend)

m2_t0 <- lm(rs_change_spend ~ rs_change_spend_1 + election_year*lpr_1, 
            data = sub_gov_spend)

m3_t0 <- lm(rs_change_spend ~ rs_change_spend_1 + election_year*lpr_1 + iso2c, 
                  data = sub_gov_spend)

m4_t0 <- lm(rs_change_spend ~ rs_change_spend_1 + election_year*lpr_1 + 
                execrlc + polconiii + fixed_exchange, 
            data = sub_gov_spend)

# Debt
m5_t0 <- lm(rs_change_debt ~ rs_change_debt_1 + election_year*lpr_1 + 
                execrlc + polconiii + fixed_exchange, 
            data = sub_debt)

#### Post-Election Year ####
# Debt
m1_t1 <- lm(rs_change_debt ~ rs_change_debt_1 + election_year_1 + lpr, 
            data = sub_debt)

m2_t1 <- lm(rs_change_debt ~ rs_change_debt_1 + election_year_1*lpr, 
            data = sub_debt)

m3_t1 <- lm(rs_change_debt ~ rs_change_debt_1 + election_year_1*lpr + iso2c, 
            data = sub_debt)

m4_t1 <- lm(rs_change_debt ~ rs_change_debt_1 + election_year_1*lpr + execrlc + 
                polconiii + fixed_exchange + iso2c, 
            data = sub_debt)

m5_t1 <- lm(rs_change_debt ~ rs_change_debt_1 + rs_change_spend + 
                election_year_1*lpr + execrlc + 
                polconiii + fixed_exchange, 
            data = sub_debt)

m6_t1 <- lm(rs_change_debt ~ rs_change_debt_1 + rs_change_spend_1 + 
                election_year_1*lpr + execrlc + 
                polconiii + fixed_exchange, 
            data = sub_debt)

# Spending
m7_t1 <- lm(rs_change_spend ~ rs_change_spend_1 + election_year_1 + lpr + 
                execrlc + polconiii + fixed_exchange, data = sub_gov_spend)


# ----------------- Other Tests ---------------------- #
plm1 <- pbltest(rs_change_spend ~ election_year + lpr_1 + rs_change_spend_1, 
             data = sub_gov_spend)

ols <- lm(rs_change_spend ~ election_year + lpr_1 + rs_change_spend_1, 
            data = sub_gov_spend)

plm2 <- plm(rs_change_debt ~ election_year_1 + rs_change_debt_1, 
                 data = sub_debt,
                 method = 'random')

plm3 <- pwfdtest(rs_change_debt ~ election_year_1 + lpr + rs_change_debt_1, 
             data = sub_debt)

pFtest(m1_t0, ols) 
