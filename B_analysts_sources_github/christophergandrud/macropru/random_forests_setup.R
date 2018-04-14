# ---------------------------------------------------------------------------- #
# Set up Random Forests and create descriptive tables/plots
# Christopher Gandrud
# MIT LICENSE
# ---------------------------------------------------------------------------- #

# Load packages
library(rio)
library(repmis)
library(dplyr)
library(xtable)
library(DataCombine)
library(corrplot)
library(Amelia)

# Set working directory
possibles <- "/git_repositories/macropru/"
set_valid_wd(possibles)

# Load combined data set
main <- import('data/main_combined.csv')

# Inequality transformations -------
main$redist_absolute <- main$gini_market - main$gini_net
main$redist_relative <- (main$redist_absolute / main$gini_market) * 100
main$gini_diff_red <- main$gini_market - main$redist

# Find variable correlations ----------------
keepers <- c('any_tighten', 'lag_cumsum_any_tighten',
             'gdp_growth', 'gdp_per_capita', 'inflation', 
             'bis_housing_change',  'bis_credit_change', 'cbi',
             'executive_election_4qt', 'executive_election_4qt_after',
             'cb_policy_rate',
             'cb_policy_rate_change', 'gini_net',
             'redist_absolute', 'uds_mean', 'us_fed_funds'
             )

keeper_labels <- c('Any MPR Tightening', 'Cum. Tight. (lag)', 
                   'GDP Growth', 'GDP/Capita', 'Inflation',
                   'Housing Chng', 'Credit Chng', 'CBI', 'Election Period',
                   'Post-Election',
                   'Policy Rt', 'Policy Rt Chng', 
                   'Gini Net', 'Abs. Redist.', 'UDS', 'Fed Funds Rate')

# Correlation plot
subbed <- main[, keepers[-1]]
names(subbed) <- keeper_labels[-1]
iv_correlations <- cor(subbed, use = 'complete.obs')

pdf(file = 'papers/figures/corrplot_iv.pdf')
    corrplot::corrplot(iv_correlations, type = 'lower', method = "ellipse")
dev.off()

# Missingness map -----------------
data_for_missing <- main[, c('country', 'year_quarter', 
                             'finstress_qt_mean', 'mapp', 'mof',
                             'domestic_credit',
                             keepers[-1])]

pdf(file = 'papers/figures/missing_map.pdf')
    missmap(data_for_missing, csvar = 'country', tsvar = 'year_quarter',
            main = '', y.cex = 0.5, x.cex = 0.5)
dev.off()

# Set as factors -----------------
main$country <- factor(main$country)
#main$year <- as.integer(main$year)
# main$quarter <- as.factor(main$quarter)
main$executive_election_4qt <- factor(main$executive_election_4qt)


main$mapp_cb_chair <- NA
main$mapp_cb_chair[main$mapp < 3 & !is.na(main$mapp)] <- 0
main$mapp_cb_chair[main$mapp >= 3] <- 1

main$mof_cb_chair <- NA
main$mof_cb_chair[main$mof < 3 & !is.na(main$mof)] <- 0
main$mof_cb_chair[main$mof >= 3] <- 1

main$mapp_cb_chair <- factor(main$mapp_cb_chair)
main$mapp <- factor(main$mapp)
main$mof <- factor(main$mof)
main$mipp <- factor(main$mipp)

# Rescale DV to get estimates in a sensible interpretable direction
main$any_tighten[main$any_tighten == 0] <- 'No Change'
main$any_tighten[main$any_tighten == 1] <- 'Tighten'

main$any_loosen[main$any_loosen == 0] <- 'No Change'
main$any_loosen[main$any_loosen == 1] <- 'Loosen'

main$any_tighten <- factor(main$any_tighten, 
                           levels = c('Tighten', 'No Change'))
main$any_loosen <- factor(main$any_loosen, 
                          levels = c('Loosen', 'No Change'))

FindDups(main, c('country', 'year_quarter'))

# Only democracies ------
#dem <- main %>% filter(polity2 > 5)

dem = main

# Keep complete cases
dem_no_na_1 <- dem %>% DropNA(keepers)

# Table of country quarter sample used in the models -----------
the_sample <- dem_no_na_1 %>% group_by(country) %>%
    summarise(`First Year` = min(year),
              `Last Year` = max(year)) %>%
    rename(Country = country)

print(xtable(the_sample, 
             caption = 'Country Quarter-Year Sample Included in the Random Forests After Deleting Cases with Missing Values',
             label = 'sampcases'),
      caption.placement = 'top',
      include.rownames = FALSE,
      file = 'papers/tables/rf_sample.tex')

# Sample size
Tighten <- summary(dem_no_na_1$any_tighten)[[1]]
Loosen <- summary(dem_no_na_1$any_loosen)[[1]]
Total <- nrow(dem_no_na_1)

print(xtable(data.frame(Tighten, Loosen, Total),
             caption = 'Number of Events and Total Observations for the Random Forests Estimation Sample',
             label = 'sampsize'),
      caption.placement = 'top',
      include.rownames = FALSE,
      file = 'papers/tables/rf_sample_size.tex'
)
