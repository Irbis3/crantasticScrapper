# ---------------------------------------------------------------------------- #
# Predictive plots
# Christopher Gandrud
# MIT License
# ---------------------------------------------------------------------------- #

# Load packages
library(repmis)
library(dplyr)
library(ggplot2)
library(gridExtra)
library(simGLM)

# Set working directory
possibles <- "/git_repositories/macropru/"
set_valid_wd(possibles)

# Estimate models -------
source('analysis/regressions_v2_democracies.R')

# Simulate and plot for range of fitted CBI and growth values --------
fitted_cbi_tighten <- with(dem, 
                           expand.grid(
                               cbi = unique(cbi)[!is.na(unique(cbi))],
                               lag_cumsum_any_tighten = mean(lag_cumsum_any_tighten,
                                                             na.rm = TRUE),
                               gdp_growth = quantile(gdp_growth, probs = 0.9, na.rm = T),
                               inflation = mean(inflation, na.rm = TRUE)
                           )
)

fitted_cbi_loosen <- with(dem, 
                           expand.grid(
                               cbi = unique(cbi)[!is.na(unique(cbi))],
                               lag_cumsum_any_tighten = mean(lag_cumsum_any_tighten,
                                                             na.rm = TRUE),
                               gdp_growth = quantile(gdp_growth, probs = 0.1, na.rm = T),
                               inflation = mean(inflation, na.rm = TRUE)
                           )
)

cbi_tighten <- sim_glm(t4, newdata = fitted_cbi_tighten, x_coef = 'cbi', model = 'logit') +
                        xlab('') + 
                        ylab('Probability of Tightening MPR\n')

cbi_loosen <- sim_glm(l4, newdata = fitted_cbi_loosen, x_coef = 'cbi', model = 'logit') +
                        xlab('') + 
                        ylab('Probability of Loosening MPR\n')

pdf(file = 'figures/cbi_predictions.pdf', width = 11)
    grid.arrange(cbi_tighten, cbi_loosen, nrow = 1, 
             bottom = 'Central Bank Independence')
dev.off()


# Simulate and plot for range of fitted housing price changes and growth values --------
fitted_housing <- with(dem,
                       expand.grid(
                           lag_cumsum_any_tighten = mean(lag_cumsum_any_tighten,
                                                         na.rm = TRUE),
                           inflation = mean(inflation, na.rm = TRUE),
                           gdp_growth = c(
                               quantile(gdp_growth, probs = 0.1, na.rm = T),
                               quantile(gdp_growth, probs = 0.9, na.rm = T)
                           ),
                           bis_housing_change = unique(bis_housing_change)[
                               !is.na(unique(bis_housing_change))]
                       )
)

fitted_housing$gdp_growth <- round(fitted_housing$gdp_growth, digits = 1)

fitted_housing$`gdp_growth:bis_housing_change` <- fitted_housing$gdp_growth *
                                                    fitted_housing$bis_housing_change

bis_tighten <- sim_glm(t2, newdata = fitted_housing, x_coef = 'bis_housing_change', 
                        group_coef = 'gdp_growth', model = 'logit') +
                    xlab('\nHousing Price Change') + 
                    ylab('Probability of Tightening MPR\n') +
                    guides(fill = guide_legend(title = 'GDP Growth'),
                        color = guide_legend(title = 'GDP Growth'))

ggsave(bis_tighten, file = 'figures/bis_gdp_predictions.pdf', width = 9.72,
       height = 7.26)
