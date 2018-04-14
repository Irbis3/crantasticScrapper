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
library(stringr)

# Set working directory. Change as needed.
setwd('/git_repositories/financial_crisis_fiscal_policy/')

comb <- import('https://raw.githubusercontent.com/christophergandrud/financial_crisis_fiscal_policy/master/analysis_data/covariate_data/epfms_covariates.csv')

# Convert key varaiables to factors
comb$iso2c <- comb$iso2c %>% as.factor
comb$election_year <- comb$election_year %>% as.factor
comb$election_year_1 <- comb$election_year_1 %>% as.factor

##### Create residuals #####
# Output and Stress Gap Residuals (FinStress) ----------------------------------
m_r1 <- lm(cent_debt_gdp2005 ~ cent_debt_gdp2005_1 + mean_stress + output_gap + 
               iso2c, data = comb)

sub_debt <- comb %>% DropNA(c('cent_debt_gdp2005', 'cent_debt_gdp2005_1', 
                              'output_gap', 'mean_stress'))
sub_debt$residuals_debt <- residuals(m_r1)

sub_debt <- change(sub_debt, Var = 'residuals_debt', 
                             GroupVar = 'iso2c', TimeVar = 'year',
                             type = 'absolute', NewVar = 'rs_change_debt')

sub_debt <- slide(sub_debt, Var = 'rs_change_debt', 
                            NewVar = 'rs_change_debt_1',
                            GroupVar = 'iso2c', TimeVar = 'year')

# Estimate with Generalised Method of Moments (Arellano and Bond) --------------
sub_for_gmm <- DropNA(comb, c('cent_debt_gdp2005', 'mean_stress'))
sub_ps <- pdata.frame(sub_for_gmm, index = c('iso2c', 'year'))
m_r1_gmm <- pgmm(cent_debt_gdp2005 ~ lag(cent_debt_gdp2005, 1) + mean_stress + 
                 output_gap | lag(cent_debt_gdp2005, 2:9),
             data = sub_ps,
             effect = 'individual',
             transformation = 'd')

debt_residuals_gmm <- residuals(m_r1_gmm) %>% unlist %>% as.data.frame
debt_residuals_gmm$comb_name <- row.names(debt_residuals_gmm)
iso2c_year <- str_split_fixed(debt_residuals_gmm$comb_name, 
                              pattern = '\\.', n = 2) %>% 
                    as.data.frame(stringsAsFactors = FALSE)
debt_residuals_gmm$iso2c <- iso2c_year[, 1]
debt_residuals_gmm$year <- iso2c_year[, 2]
debt_residuals_gmm <- debt_residuals_gmm[, c(3:4, 1)]
names(debt_residuals_gmm) <- c('iso2c', 'year', 'gmm_resid')

debt_residuals_gmm <- change(debt_residuals_gmm, Var = 'gmm_resid', 
                             GroupVar = 'iso2c', TimeVar = 'year',
                             type = 'absolute', NewVar = 'rs_change_debt_gmm')

debt_residuals_gmm <- slide(debt_residuals_gmm, Var = 'rs_change_debt_gmm', 
                        NewVar = 'rs_change_debt_gmm_1',
                        GroupVar = 'iso2c', TimeVar = 'year')

# Compare residuals estimation method ------------------------------------------
resid_examine <- merge(sub_debt, debt_residuals_gmm, by = c('iso2c', 'year'), 
                       all.x = TRUE)

plot(resid_examine$residuals_debt, resid_examine$gmm_resid)
plot(resid_examine$rs_change_debt, resid_examine$rs_change_debt_gmm)


# Estimate models with GMM estimated residuals ---------------------------------
m2_t1_gmm <- lm(rs_change_debt_gmm ~ rs_change_debt_gmm_1 + election_year_1*lpr +
                    iso2c, 
                data = resid_examine)

m3_t1_gmm <- lm(rs_change_debt_gmm ~ rs_change_debt_gmm_1 + election_year_1*lpr, 
            data = resid_examine)

dropped_outliers <- resid_examine %>% filter(country != 'Greece' & 
                                            country != 'Iceland')

m4_t1_gmm <- lm(rs_change_debt_gmm ~ rs_change_debt_gmm_1 + election_year_1*lpr + 
                    iso2c, 
            data = dropped_outliers)

m5_t1_gmm <- lm(rs_change_debt_gmm ~ rs_change_debt_gmm_1 + 
                election_year_1*lpr + bond_spread + execrlc + 
                polconiii + fixed_exchange, 
            data = dropped_outliers)

plot_me(m5_t1_gmm, 'election_year_11', 'lpr')
