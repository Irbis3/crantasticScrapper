# ---------------------------------------------------------------------------- #
# Create tables from results of spending_regressions.R
# Version 2
# Christopher Gandrud
# MIT License
# ---------------------------------------------------------------------------- #

# Load required packages
library(dplyr)
library(DataCombine)
library(stargazer)
library(xtable)

# Set working directory. Change as needed.
setwd('/git_repositories/financial_crisis_fiscal_policy/')

# Run regressions
source('analysis_data/spending_regressions_v2.R')

# Residual Regressions
stargazer(m_r1, m_r1_econ, m_r2_basic, m_r2, m_r2_econ_basic, m_r2_econ,
          dep.var.labels = c('Debt', 
                             'Econ. Spend', 
                             'Debt Resid.',
                             'Econ. Spend Resid.'),
          covariate.labels = c('Debt$_{t-1}$', 
                               'Spending$_{t-1}$',
                               'Output Gap',
                               'Debt Resid.$_{t-1}$',
                               'Econ. Spend Resid.$_{t-1}$',
                               'Banking Crisis',
                               'Perceived Stress Intensity',
                               'Crisis * Intensity'),
          omit = 'iso2c', omit.labels = 'country fixed effects',
          float = F,
          df = F,
          font.size = 'tiny',
          out = 'paper/tables/debt_residual_regress.tex')

# Financail stress Regressions, election year
stargazer(m1_t0, m2_t0, m3_t0, m4_t0, m5_t0,
          dep.var.labels = c('$\\Delta$ Off-Trend Spending',
                             '$\\Delta$ Off-Trend Debt'),
          covariate.labels = c('$\\Delta$ Off-Trend Spend.$_{t-1}$',
                               '$\\Delta$ Off-Trend Debt$_{t-1}$',
                               'Election Yr.', 'Loss Prob.$_{t-1}$', 
                               'Econ Ideology', 'Political Constraints',
                               'Fixed FX',
                               'Election Yr. * Loss Prob.'),
          omit = 'iso2c', omit.labels = 'country fixed effects',
          float = F,
          df = F,
          font.size = 'tiny',
          notes = ('Standard errors in parentheses.'),
          out = 'paper/tables/stress_regress_t0.tex'
)

# Financail stress Regressions, post-election year
stargazer(m1_t1, m2_t1, m3_t1, m4_t1, m5_t1, m6_t1, m7_t1,
          dep.var.labels = c('$\\Delta$ Off-Trend Debt',
                             '$\\Delta$ Off-Trend Spending'),
          covariate.labels = c('$\\Delta$ Off-Trend Debt$_{t-1}$',
                               '$\\Delta$ Off-Trend Spend',
                               '$\\Delta$ Off-Trend Spend$_{t-1}$',
                               'Post-Election Yr.', 'Loss Prob.', 
                               'Econ Ideology', 'Political Constraints',
                               'Fixed FX',
                               'Post-Election Yr. * Loss Prob.'),
          omit = 'iso2c', omit.labels = 'country fixed effects',
          float = F,
          df = F,
          font.size = 'tiny',
          notes = ('Standard errors in parentheses.'),
          out = 'paper/tables/stress_regress_t1.tex'
)

#### Online Appendix #### 
# Country sample
countries <- sub_debt %>% arrange(country, year) %>%
                DropNA('lpr') %>%
                rename(Country = country) %>%
                select(Country) %>%
                unique %>% as.vector

xtable(countries, caption = 'Regressions Country Sample', 
       label = 'country_sample') %>% 
    print(include.rownames = F,
        size = 'footnotesize',
        caption.placement = 'top',
        file = 'paper/tables/debt_reg_sample.tex')


# OECD General Government Liabilities Rather than WDI Central Government Debt
rm(list = ls())
source('analysis_data/appendix_regressions.R')

stargazer(m5_t0_liab, m5_t1_liab,
          dep.var.labels = '$\\Delta$ Off-Trend Central Gov. Liabilities',
          covariate.labels = c('$\\Delta$ Off-Trend Liabilities$_{t-1}$',
                               'Election Yr.', 
                               'Loss Prob.$_{t-1}$',
                               'Post-Election Yr.', 
                               'Loss Prob.', 
                               'Econ Ideology', 'Political Constraints',
                               'Fixed FX',
                               'Election Yr. * Loss Prob.$_{t-1}$',
                               'Post-Election Yr. * Loss Prob.'),
          omit = 'iso2c', omit.labels = 'country fixed effects',
          float = F,
          df = F,
          font.size = 'tiny',
          notes = ('Standard errors in parentheses.'),
          out = 'paper/tables/liab_regressions.tex'
)
