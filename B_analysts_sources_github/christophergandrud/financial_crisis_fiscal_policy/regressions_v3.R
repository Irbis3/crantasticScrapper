# ---------------------------------------------------------------------------- #
# Elections, Electoral Loss Probabilities, and Government Fiscal Policies
# Version 3
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
# Output and Stress Gap Residuals
m_r1 <- lm(cent_debt_gdp2005 ~ cent_debt_gdp2005_1 + mean_stress + output_gap + 
               iso2c, data = comb)

sub_debt <- comb %>% DropNA(c('cent_debt_gdp2005', 'cent_debt_gdp2005_1', 
                              'output_gap', 'mean_stress'))
sub_debt$residuals_debt <- residuals(m_r1)
sub_debt <- slide(sub_debt, Var = 'residuals_debt', 
                  NewVar = 'residuals_debt_1',
                  GroupVar = 'country', TimeVar = 'year')

sub_debt$rs_change_debt <- sub_debt$residuals_debt -
                                sub_debt$residuals_debt_1 

sub_debt <- slide(sub_debt, Var = 'rs_change_debt', 
                  NewVar = 'rs_change_debt_1',
                  GroupVar = 'country', TimeVar = 'year')

# ------------------------- Econ Spending Residuals -------------- #
#### Create Total Spending Residuals ####
m_r1_econ <- lm(gov_econ_spend_gdp2005 ~ gov_econ_spend_gdp2005_1 +
                    mean_stress + output_gap + iso2c,
                data = comb)

sub_gov_spend <- comb %>% DropNA(c('gov_econ_spend_gdp2005', 
                                   'gov_econ_spend_gdp2005_1',
                                   'mean_stress',
                                   'output_gap'))
sub_gov_spend$residuals_spend <- residuals(m_r1_econ)
sub_gov_spend <- slide(sub_gov_spend, Var = 'residuals_spend', 
                       NewVar = 'residuals_spend_1',
                       GroupVar = 'country', TimeVar = 'year')

sub_gov_spend$rs_change_spend <- sub_gov_spend$residuals_spend -
                                    sub_gov_spend$residuals_spend_1 

sub_gov_spend <- slide(sub_gov_spend, Var = 'rs_change_spend', 
                       NewVar = 'rs_change_spend_1',
                       GroupVar = 'country', TimeVar = 'year')

# Merge change in off-trend spending to off-trend debt
to_merge <- sub_gov_spend %>% dplyr::select(iso2c, year, rs_change_spend, 
                                     rs_change_spend_1)

sub_debt <- merge(sub_debt, to_merge, by = c('iso2c', 'year'), 
                  all.x = T)

# Drop outliers
dropped_outliers <- sub_debt %>% filter(country != 'Greece' & 
                                            country != 'Iceland')

# ------------------------------- Regressions -------------------------------- #
### Election Year #### 
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
m1_t1 <- lm(rs_change_debt ~ rs_change_debt_1 + election_year_1 + lpr + iso2c, 
            data = sub_debt)

m2_t1 <- lm(rs_change_debt ~ rs_change_debt_1 + election_year_1*lpr + iso2c, 
            data = sub_debt)

m3_t1 <- lm(rs_change_debt ~ rs_change_debt_1 + election_year_1*lpr, 
            data = sub_debt)

m4_t1 <- lm(rs_change_debt ~ rs_change_debt_1 + election_year_1*lpr + 
                bond_spread + execrlc + polconiii + fixed_exchange, 
            data = sub_debt)

m5_t1 <- lm(rs_change_debt ~ rs_change_debt_1 + 
                election_year_1*lpr + bond_spread + execrlc + 
                polconiii + fixed_exchange, 
            data = dropped_outliers)

m6_t1 <- lm(rs_change_debt ~ rs_change_debt_1 + rs_change_spend + 
                election_year_1*lpr + bond_spread + execrlc + 
                polconiii + fixed_exchange, 
            data = dropped_outliers)

m7_t1 <- lm(rs_change_debt ~ rs_change_debt_1 + rs_change_spend_1 + 
                election_year_1*lpr + bond_spread + execrlc + 
                polconiii + fixed_exchange, 
            data = dropped_outliers)

# Spending
m8_t1 <- lm(rs_change_spend ~ rs_change_spend_1 + election_year_1 + lpr + 
                execrlc + polconiii + fixed_exchange, data = sub_gov_spend)

