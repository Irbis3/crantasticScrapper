# ---------------------------------------------------------------------------- #
# Preliminary Analysis (full sample)
# Christopher Gandrud
# MIT LICENSE
# ---------------------------------------------------------------------------- #

# Load packages
library(rio)
library(repmis)
library(dplyr)
library(DataCombine)
library(arm)
library(texreg)
#library(Zelig)

# Set working directory
possibles <- "/git_repositories/macropru/"
set_valid_wd(possibles)

# Load combined data set
main <- import('data/main_combined.csv')

# Set as factors
main$country <- as.factor(main$country)
main$year <- as.factor(main$year)
main$quarter <- as.factor(main$quarter)
main$executive_election_4qt <- as.factor(main$executive_election_4qt)
main$executive_election_4qt_after <- as.factor(main$executive_election_4qt_after)

FindDups(main, c('country', 'year_quarter'))

# Simple logistic regressions tightening (no BIS variables) --------
t1 <- bayesglm(any_tighten ~ lag_cumsum_any_tighten + gdp_growth + inflation +
                   us_fed_funds +
                   finstress_qt_mean +
                   country + year
               , data = main, family = binomial(link = 'logit'))

t2 <- bayesglm(any_tighten ~ lag_cumsum_any_tighten + gdp_growth + inflation +
                   cb_policy_rate + cb_policy_rate_change +
                   country + year
               , data = main, family = binomial(link = 'logit'))

t3 <- bayesglm(any_tighten ~ lag_cumsum_any_tighten + gdp_growth + inflation +
                   uds_mean + cbi + 
                   country + year
               , data = main, family = binomial(link = 'logit'))

t4 <- bayesglm(any_tighten ~ lag_cumsum_any_tighten + gdp_growth + inflation +
                   uds_mean + cbi + 
                   executive_election_4qt + 
                   country + year
               , data = main, family = binomial(link = 'logit'))

t5 <- bayesglm(any_tighten ~ lag_cumsum_any_tighten + gdp_growth + inflation +
                   uds_mean + cbi + 
                   executive_election_4qt + executive_election_4qt_after +
                   country + year
               , data = main, family = binomial(link = 'logit'))

t6 <- bayesglm(any_tighten ~ lag_cumsum_any_tighten + gdp_growth + inflation +
                   uds_mean +
                   gini_net +
                   country + year
               , data = main, family = binomial(link = 'logit'))

t7 <- bayesglm(any_tighten ~ lag_cumsum_any_tighten + gdp_growth + inflation +
                   uds_mean + cbi +
                   domestic_credit_change +
                   country + year
               , data = main, family = binomial(link = 'logit'))


# Create display table for tightening ---------
est_tighten <- list(t1, t2, t3, t4, t5, t6, t7)

screenreg(est_tighten,
          omit.coef = 'country|year')

texreg(est_tighten,
       custom.model.names = c('A1', 'A2','A3','A4','A5','A6','A7'),
       fontsize = 'tiny',
       omit.coef = 'country|year',
       table = FALSE,
       file = 'papers/tables/stepwise_bayeslogit_no_bis.tex'
)

# Democracy and elections interaction 
t8 <- bayesglm(any_tighten ~ lag_cumsum_any_tighten + gdp_growth + inflation +
                   cbi + 
                   uds_mean*executive_election_4qt + uds_mean*executive_election_4qt_after +
                   country + year
               , data = main, family = binomial(link = 'logit'))


# Simple logistic regressions tightening (BIS variables) --------
tb1 <- bayesglm(any_tighten ~ lag_cumsum_any_tighten + gdp_growth + inflation +
                   us_fed_funds +
                   finstress_qt_mean +
                   bis_housing_change + bis_credit_change +
                   country + year
               , data = main, family = binomial(link = 'logit'))

tb2 <- bayesglm(any_tighten ~ lag_cumsum_any_tighten + gdp_growth + inflation +
                   bis_housing_change + bis_credit_change +
                   cb_policy_rate + cb_policy_rate_change +
                   country + year
               , data = main, family = binomial(link = 'logit'))

tb3 <- bayesglm(any_tighten ~ lag_cumsum_any_tighten + gdp_growth + inflation +
                    bis_housing_change + bis_credit_change +
                    uds_mean + cbi + 
                   country + year
               , data = main, family = binomial(link = 'logit'))

tb4 <- bayesglm(any_tighten ~ lag_cumsum_any_tighten + gdp_growth + inflation +
                    bis_housing_change + bis_credit_change +
                    uds_mean + cbi + 
                   executive_election_4qt + 
                   country + year
               , data = main, family = binomial(link = 'logit'))

tb5 <- bayesglm(any_tighten ~ lag_cumsum_any_tighten + gdp_growth + inflation +
                    bis_housing_change + bis_credit_change +
                    uds_mean + cbi + 
                   executive_election_4qt + executive_election_4qt_after +
                   country + year
               , data = main, family = binomial(link = 'logit'))

tb6 <- bayesglm(any_tighten ~ lag_cumsum_any_tighten + gdp_growth + inflation +
                    bis_housing_change + bis_credit_change +
                    uds_mean +
                   gini_net +
                   country + year
               , data = main, family = binomial(link = 'logit'))

no_bis <- DropNA(main, c('bis_housing_change', 'bis_credit_change'))

tb7 <- bayesglm(any_tighten ~ lag_cumsum_any_tighten + gdp_growth + inflation +
                    uds_mean + cbi +
                   country + year
               , data = no_bis, family = binomial(link = 'logit'))


# Create display table for tightening ---------
est_tighten <- list(tb1, tb2, tb3, tb4, tb5, tb6, tb7)

screenreg(est_tighten,
          omit.coef = 'country|year')

texreg(est_tighten,
       custom.model.names = c('B1', 'B2','B3','B4','B5','B6','B7'),
       fontsize = 'tiny',
       omit.coef = 'country|year',
       table = FALSE,
       file = 'papers/tables/stepwise_bayeslogit_bis_included.tex'
)

# Simple logistic regressions loosening -----------------------------------
l1 <- bayesglm(any_loosen ~ lag_cumsum_any_tighten + gdp_growth + inflation +
                   finstress_qt_mean + bis_housing_change +
                   country + year
               , data = main, family = binomial(link = 'logit'))

l2 <- bayesglm(any_loosen ~ lag_cumsum_any_tighten + gdp_growth + inflation +
                   gini_net + 
                   country + year
               , data = main, family = binomial(link = 'logit'))

l3 <- bayesglm(any_loosen ~ lag_cumsum_any_tighten + gdp_growth + inflation +
                   cbi +
                   country + year
               , data = main, family = binomial(link = 'logit'))


l4 <- bayesglm(any_loosen ~ lag_cumsum_any_tighten + gdp_growth + inflation +
                   cbi +
                   execrlc + executive_election_4qt +
                   country + year
               , data = main, family = binomial(link = 'logit'))

l5 <- bayesglm(any_loosen ~ lag_cumsum_any_tighten + gdp_growth + inflation +
                   cbi +
                   fiscal_trans_gfs +
                   country + year
               , data = main, family = binomial(link = 'logit'))

l6 <- bayesglm(any_loosen ~ lag_cumsum_any_tighten + gdp_growth + inflation +
                   cbi +
                   domestic_credit_change +
                   country + year
               , data = main, family = binomial(link = 'logit'))

# Create display table for loosening ---------
est_loosen <- list(l1, l2, l3, l4, l5, l6)

screenreg(est_loosen,
          omit.coef = 'country')

texreg(est_loosen,
       omit.coef = 'factor',
       table = FALSE)
