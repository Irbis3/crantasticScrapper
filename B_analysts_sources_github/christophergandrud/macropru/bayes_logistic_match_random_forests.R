# ---------------------------------------------------------------------------- #
# Bayes Logistics to Match Random Forests
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

# Set working directory
possibles <- "/git_repositories/macropru/"
set_valid_wd(possibles)

# Load combined data set
source('analysis/random_forests_setup.R')

main$country <- as.factor(main$country)
main$year <- as.factor(main$year)
main$quarter <- as.factor(main$quarter)
main$executive_election_4qt <- as.factor(main$executive_election_4qt)
main$executive_election_4qt_after <- as.factor(main$executive_election_4qt_after)

# Classical logit for tightening ------------
glm_1 <- glm(any_tighten ~ lag_cumsum_any_tighten + gdp_growth +
                 bis_housing_change +
                 bis_credit_change +
                 inflation +
                 us_fed_funds +
                 #finstress_qt_mean +
                 #gini_market +
                 gini_net +
                 redist_relative +
                 executive_election_4qt +
                 executive_election_4qt_after +
                 cb_policy_rate + cb_policy_rate_change +
                 cbi +
                 #polconv +
                 uds_mean +
                 country +
                 year # + quarter
             , data = dem_no_na_1, family = binomial(link = 'logit'))

glm_2 <- glm(any_loosen ~ lag_cumsum_any_tighten + gdp_growth +
                 bis_housing_change +
                 bis_credit_change +
                 inflation +
                 us_fed_funds +
                 #finstress_qt_mean +
                 #gini_market +
                 gini_net +
                 redist_relative +
                 executive_election_4qt +
                 executive_election_4qt_after +
                 cb_policy_rate + cb_policy_rate_change +
                 cbi +
                 #polconv +
                 uds_mean +
                 country +
                 year # + quarter
             , data = dem_no_na_1, family = binomial(link = 'logit'))

texreg(list(glm_1, glm_2),
        omit.coef = 'country|year',
        custom.model.names = c('Tightening MPR', 'Loosening MPR'),
        table = FALSE,
        file = 'papers/tables/garbage_can_ordinary_logit.tex'
)

# Bayes logit for tightening ------------
bglm_1 <- bayesglm(any_tighten ~ lag_cumsum_any_tighten + gdp_growth +
                       bis_housing_change +
                       bis_credit_change +
                       inflation +
                       us_fed_funds +
                       #finstress_qt_mean +
                       #gini_market +
                       gini_net +
                       redist_relative +
                       executive_election_4qt +
                       executive_election_4qt_after +
                       cb_policy_rate + cb_policy_rate_change +
                       cbi +
                       #polconv +
                       uds_mean +
                       country +
                       year # + quarter
                   , data = dem_no_na_1, family = binomial(link = 'logit'))

bglm_2 <- bayesglm(any_loosen ~ lag_cumsum_any_tighten + gdp_growth +
                 bis_housing_change +
                 bis_credit_change +
                 inflation +
                 us_fed_funds +
                 #finstress_qt_mean +
                 #gini_market +
                 gini_net +
                 redist_relative +
                 executive_election_4qt +
                 executive_election_4qt_after +
                 cb_policy_rate + cb_policy_rate_change +
                 cbi +
                 #polconv +
                 uds_mean +
                 country +
                 year # + quarter
                   , data = dem_no_na_1, family = binomial(link = 'logit'))

texreg(list(bglm_1, bglm_2),
          omit.coef = 'country|year',
          custom.model.names = c('Tightening MPR', 'Loosening MPR'),
          table = FALSE,
          file = 'papers/tables/garbage_can_bayes_logit.tex'
          )
