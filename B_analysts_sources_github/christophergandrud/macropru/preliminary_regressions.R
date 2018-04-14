# ---------------------------------------------------------------------------- #
# Preliminary Analysis
# Christopher Gandrud
# MIT LICENSE
# ---------------------------------------------------------------------------- #

# Load packages
library(rio)
library(repmis)
library(dplyr)
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


# Only democracies ------
dem <- main %>% filter(polity2 > 5)

# Simple logistic regressions tightening --------
t1 <- glm(any_tighten ~ lag_cumsum_any_tighten + finstress_qt_mean + executive_election_4qt +
              country + year + quarter
          , data = dem, family = 'binomial')

t2 <- glm(any_tighten ~ lag_cumsum_any_tighten + finstress_qt_mean + cbi +
              country + year + quarter
          , data = dem, family = 'binomial')

t3 <- glm(any_tighten ~ lag_cumsum_any_tighten + finstress_qt_mean*cbi +
              country + year + quarter
          , data = dem, family = 'binomial')

t4 <- glm(any_tighten ~ lag_cumsum_any_tighten + finstress_qt_mean + cbi + execrlc +
              country + year + quarter
          , data = dem, family = 'binomial')

t5 <- glm(any_tighten ~ lag_cumsum_any_tighten + finstress_qt_mean + cbi +  fiscal_trans_gfs +
              country + year + quarter
          , data = dem, family = 'binomial')

t6 <- glm(any_tighten ~ lag_cumsum_any_tighten + finstress_qt_mean + cbi + domestic_credit_change +
              country + year + quarter
          , data = dem, family = 'binomial')

t7 <- glm(any_tighten ~ lag_cumsum_any_tighten + finstress_qt_mean + cbi + gdp_growth +
              country + year + quarter
          , data = dem, family = 'binomial')

t8 <- glm(any_tighten ~ lag_cumsum_any_tighten + finstress_qt_mean + cbi + inflation +
              country + year + quarter
          , data = dem, family = 'binomial')

# Simple logistic regressions loosening -------
l1 <- glm(any_loosen ~ lag_cumsum_any_tighten + fiscal_trans_gfs + finstress_qt_mean +
              country + year + quarter
          , data = dem, family = 'binomial')

l2 <- glm(any_loosen ~ lag_cumsum_any_tighten + fiscal_trans_gfs + executive_election_4qt +
              country + year + quarter
          , data = dem, family = 'binomial')

l3 <- glm(any_loosen ~ lag_cumsum_any_tighten + fiscal_trans_gfs + executive_election_4qt + cbi +
              country + year + quarter
          , data = dem, family = 'binomial')

l4 <- glm(any_loosen ~ lag_cumsum_any_tighten + fiscal_trans_gfs + executive_election_4qt + execrlc +
              country + year + quarter
          , data = dem, family = 'binomial')

l5 <- glm(any_loosen ~ lag_cumsum_any_tighten + fiscal_trans_gfs + 
              executive_election_4qt + domestic_credit_change +
              country + year + quarter
          , data = dem, family = 'binomial')

l6 <- glm(any_loosen ~ lag_cumsum_any_tighten + fiscal_trans_gfs + 
              executive_election_4qt + gdp_growth +
              country + year + quarter
          , data = dem, family = 'binomial')

l7 <- glm(any_loosen ~ lag_cumsum_any_tighten + fiscal_trans_gfs + 
              executive_election_4qt + inflation +
              country + year + quarter
          , data = dem, family = 'binomial')

l8 <- glm(any_loosen ~ lag_cumsum_any_tighten + finstress_qt_mean + 
              executive_election_4qt +
              country + year + quarter
          , data = dem, family = 'binomial')

# Rare logistic Regression
#r_t3 <- zelig(any_tighten ~ finstress_qt_mean + cbi +
#              country + year
#          , data = dem, model = 'relogit', cite = F)


#r_l3 <- zelig(any_loosen ~ fiscal_trans_gfs + cbi +
#              country + year
#          , data = dem, model = 'relogit', cite = F)
