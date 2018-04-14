# ---------------------------------------------------------------------------- #
# Elections, Electoral Loss Probabilities, and Government Fiscal Policies
# Online Appendix Material
# Version 2
# Christopher Gandrud
# MIT License
# ---------------------------------------------------------------------------- #

library(rio)
library(dplyr)
library(lubridate)
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
m_r1_liab <- lm(gov_liabilities_gdp2005 ~ gov_liabilities_gdp2005_1 + output_gap + 
               iso2c, data = comb)

sub_gov_liab <- comb %>% DropNA(c('gov_liabilities_gdp2005_1', 'output_gap'))
sub_gov_liab$residuals_output_liab <- residuals(m_r1_liab)
sub_gov_liab <- slide(sub_gov_liab, Var = 'residuals_output_liab', 
                      NewVar = 'residuals_output_liab_1',
                      GroupVar = 'country', TimeVar = 'year')

# Financial Stress Residuals
m_r2_liab <- lm(residuals_output_liab ~ residuals_output_liab_1 + 
                    lv_bank_crisis*mean_stress, data = sub_gov_liab)
sub_gov_liab <- sub_gov_liab %>% DropNA(c('residuals_output_liab_1', 
                                          'lv_bank_crisis', 'mean_stress'))
sub_gov_liab$residuals_stress_liab <- residuals(m_r2_liab)

sub_gov_liab <- slide(sub_gov_liab, Var = 'residuals_stress_liab', 
                      NewVar = 'residuals_stress_liab_1',
                      GroupVar = 'country', TimeVar = 'year')

sub_gov_liab$rs_change_liab <- sub_gov_liab$residuals_stress_liab -
    sub_gov_liab$residuals_stress_liab_1 

sub_gov_liab <- slide(sub_gov_liab, Var = 'rs_change_liab', 
                      NewVar = 'rs_change_liab_1',
                      GroupVar = 'country', TimeVar = 'year')

# ------------------------------- Regressions -------------------------------- #
# OECD General Govermnment Liabilities

# Election year
m5_t0_liab <- lm(rs_change_liab ~ rs_change_liab_1 + election_year*lpr_1 + 
                          execrlc + polconiii + fixed_exchange, 
                data = sub_gov_liab)

# Post-election year
m5_t1_liab <- lm(rs_change_liab ~ rs_change_liab_1 + election_year_1*lpr + 
                     execrlc + polconiii + fixed_exchange, 
                  data = sub_gov_liab)
