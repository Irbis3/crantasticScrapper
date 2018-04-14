# ---------------------------------------------------------------------------- #
# Descriptive plots
# Christopher Gandrud
# MIT License
# ---------------------------------------------------------------------------- #

# Load packages
library(repmis)
library(rio)
library(dplyr)
library(tidyr)
library(DataCombine)
library(ggplot2)

# Set working directory
possibles <- "/git_repositories/macropru/"
set_valid_wd(possibles)

# Load combined data set
main <- import('data/main_combined.csv')

FindDups(main, c('country', 'year_quarter'))

# Find cumulative loosening
main <- main %>% arrange(country, year_quarter) %>% group_by(country) %>%
                    mutate(cumsum_any_loosen = cumsum(any_loosen))

main$country[main$country == 'Bolivia, Plurinational State of'] <- 'Bolivia'
main$country[main$country == 'Taiwan, Province of China'] <- 'Taiwan'
main$country[main$country == 'Russian Federation'] <- 'Russia'
main$country[main$country == 'United Arab Emirates'] <- 'UAE'
main$country[main$country == 'Macedonia, the former Yugoslav Republic of'] <- 'Macedonia'

dem <- main

# Cummulative tightening by country --------------
dem_cumsum <- dem %>% dplyr::select(country, year, year_quarter, cumsum_any_tighten, 
                             cumsum_any_loosen)

dem_cumsum_gathered <- dem_cumsum %>% gather(policy_type, cum_sum, 4:5)

dem_cumsum_gathered$policy_type[dem_cumsum_gathered$policy_type == 'cumsum_any_loosen'] <- 'Loosen'
dem_cumsum_gathered$policy_type[dem_cumsum_gathered$policy_type == 'cumsum_any_tighten'] <- 'Tighten'

dem_cumsum_gathered$policy_type <- factor(dem_cumsum_gathered$policy_type, 
                                 levels(factor(dem_cumsum_gathered$policy_type))[c(2, 1)])

# Plot cumulative tightening by democratic country
plot_mpr <- ggplot(dem_cumsum_gathered, aes(year_quarter, cum_sum, 
                                            colour = policy_type,
                                            linetype = policy_type)) +
    geom_line() +
    facet_wrap(~ country) +
    scale_colour_manual(values = c("#F98400", "#5BBCD6"), name = '') +
    scale_linetype(name = '') +
    scale_x_continuous(breaks = c(2000, 2005, 2010)) +
    scale_y_continuous(breaks = c(0, 10, 20)) +
    xlab('') + ylab('Cumulative Sum (from 2000)\n') +
    theme_bw()

ggsave(plot_mpr, file = 'papers/figures/cumsum_mpr.pdf', width = 14, height = 8.7)

# Overall policy loosening and tightening ----------------
dem_cumsum_overall <- dem_cumsum %>% group_by(year_quarter) %>%
    summarise(tighten_total = sum(cumsum_any_tighten),
              loosen_total = sum(cumsum_any_loosen)
              )

dem_cumsum_overall <- dem_cumsum_overall %>% gather(policy_type, cum_sum, 2:3)

dem_cumsum_overall$policy_type[dem_cumsum_overall$policy_type == 'loosen_total'] <- 'Loosen'
dem_cumsum_overall$policy_type[dem_cumsum_overall$policy_type == 'tighten_total'] <- 'Tighten'

dem_cumsum_overall$policy_type <- factor(dem_cumsum_overall$policy_type, 
                            levels(factor(dem_cumsum_overall$policy_type))[c(2, 1)])

plot_cumsum_all <- ggplot(dem_cumsum_overall, aes(year_quarter, cum_sum,
                               colour = policy_type,
                               linetype = policy_type)) +
    geom_line(size = 1) +
    scale_colour_manual(values = c("#F98400", "#5BBCD6"), name = '') +
    scale_linetype(name = '') +
    scale_x_continuous(breaks = c(2000, 2005, 2010, 2014)) +
    xlab('') + ylab('Cumulative Sum (from 2000)\n') +
    theme_bw()

ggsave(plot_cumsum_all, file = 'papers/figures/all_countries_cumsum_mpr.pdf',
       width = 7.6, height = 4.7)

