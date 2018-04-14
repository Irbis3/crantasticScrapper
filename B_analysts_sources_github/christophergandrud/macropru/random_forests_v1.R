# ---------------------------------------------------------------------------- #
# Preliminary Analysis (Random Forests)
# Christopher Gandrud
# MIT LICENSE
# ---------------------------------------------------------------------------- #

# Load packages
library(repmis)
library(randomForestSRC)
library(ggRandomForests)
library(ggplot2)

# Set working directory
possibles <- "/git_repositories/macropru/"
set_valid_wd(possibles)

# Source Set Up
source('analysis/random_forests_setup.R')


# Keep only main model variables and rename
vars_model <- c('any_tighten', 'lag_cumsum_any_tighten', 'gdp_growth',
                'us_fed_funds', 'bis_housing_change', 'cb_policy_rate',
                'uds_mean', 'bis_credit_change', 'gini_net', 
                'inflation', 'redist_relative', 'cb_policy_rate_change', 
                'cbi', 'executive_election_4qt', 'executive_election_4qt_after',
                'country', 'year')

main_var_sub <- dem_no_na_1[, vars_model]

# Minimum variable depth for tightening -------------
tighten_md_labels <- c('any_tighten', 'Cum. Tight. (lag)', 'GDP Growth',  
                       'Fed Funds', 'Housing Chng', 'CB Policy Rt',
                       'UDS', 'Credit Chng', 'Gini Net',
                       'Inflation', 'Rel. Redist.', 'CB Policy Rt Chng',
                       'CBI', 'Pre-election', 'Post-election', 
                       'Country', 'Year')

names(main_var_sub) <- tighten_md_labels

# RF for Tightening MPR -------------------------------------------------------
rt1 <- rfsrc(any_tighten ~ .
             , data = main_var_sub, importance = TRUE)

# Plot OOB errors against the growth of the forest
plot(gg_error(rt1))

# Minimum depth for tightening ------
gg_md_tighten <- gg_minimal_depth(rt1)
tighten_md <- plot(gg_md_tighten) +
               # scale_x_discrete(labels = rev(tighten_md_labels)) +
                theme_bw()

tighten_md_vimp <- plot(gg_minimal_vimp(gg_md_tighten)) + theme_bw()

tighten_imp <- plot(gg_vimp(rt1)) + 
             #   scale_colour_manual(values = c("#F98400", "#5BBCD6"), 
             #                       name = '') +
                scale_y_continuous(breaks = c(0, 0.01, 0.02)) +
                theme_bw()


ggsave(tighten_md, filename = 'papers/figures/tighten_md.pdf', 
       height = 5.82, width = 9.25)
ggsave(tighten_imp, filename = 'papers/figures/tighten_imp.pdf', 
       height = 5.82, width = 9.25)

# Order variables by minimal depth rank (exclude country)
#xvar_tighten <- gg_md_tighten$topvars[!(gg_md_tighten$topvars %in% 
#                                            c('country', 'quarter'))]

# Partial dependence for tightening -----------
# Almost all variables
xvar_tighten <- gg_md_tighten$varselect$names %>% as.character
xvar_tighten <- xvar_tighten[!(xvar_tighten %in% c('Country', 'Quarter',
                                                 'Pre-election',
                                                 'Post-election',
                                                 'ex_regime'))]

partial_bis_tighten <- plot.variable(rt1, xvar = xvar_tighten, partial = TRUE,
                             show.plots = FALSE)

partial_tighten <- plot(gg_partial(partial_bis_tighten), panel = TRUE, 
                        alpha = 0.5) +
                    geom_line() + xlab('') +
                    scale_y_continuous(limits = c(0, 0.3)) +
                    ylab('Predicted Probability of MPR Tightening\n') +
                    xlab('\nPredictor Scale') +
                    theme_bw()

ggsave(partial_tighten, filename = 'papers/figures/patial_tighten.pdf', 
       width = 12, height = 8)

# Interactions for tightening -----------
interation_tighten <- find.interaction(rt1)

interact_tighten_plot <- plot(gg_interaction(interation_tighten), 
                         panel = TRUE) + 
                        theme_bw() +
                        theme(axis.text.x = element_text(
                            angle = 90))

ggsave(interact_tighten_plot, 
       filename = 'papers/figures/interaction_check_tighten.pdf',
       width = 10, height = 9)

# RF for Tightening MPR (No BIS) -----------------------------------------------
rt2 <- rfsrc(any_tighten ~ lag_cumsum_any_tighten + gdp_growth + 
                 #   bis_housing_change +
                 #   bis_credit_change +
                 domestic_credit_change +
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
             , data = dem_no_na_1, importance = TRUE)

# Minimum variable depth for tightening (no BIS) -------------
tighten_md_labels <- c('GDP Growth', 'Country', 'Housing Price Chng', 
                       'Cum. Tight. (lag)', 'FinStress', 'Democracy',
                       'GDP/Capita', 'Gini Diff.', 'Inflation', 'CBI',
                       'Political Constraints', 'Election')

# Minimum depth
gg_md_tighten2 <- gg_minimal_depth(rt2)
tighten_md2 <- plot(gg_md_tighten2) +
    #  scale_x_discrete(labels = rev(tighten_md_labels)) +
    theme_bw()

ggsave(tighten_md2, filename = 'papers/figures/tighten_md_no_bis.pdf', 
       height = 5.82, width = 9.25)

# RF for Loosening -------------------------------------------------------------
rl1 <- rfsrc(any_loosen ~ .
             , data = main_var_sub, importance = TRUE)

# Minimum variable depth for loosening -------------
gg_md_loosen <- gg_minimal_depth(rl1)
loosen_md <- plot(gg_md_loosen) +
    #  scale_x_discrete(labels = rev(loosen_md_labels)) +
    theme_bw()

loosen_md_vimp <- plot(gg_minimal_vimp(gg_md_loosen)) + theme_bw()

loosen_imp <- plot(gg_vimp(rl1)) + 
    scale_y_continuous(breaks = c(0, 0.01, 0.02)) +
    theme_bw()


ggsave(loosen_md, filename = 'papers/figures/loosen_md.pdf', 
       height = 5.82, width = 9.25)
ggsave(loosen_imp, filename = 'papers/figures/loosen_imp.pdf', 
       height = 5.82, width = 9.25)

# Order variables by minimal depth rank (exclude country)
#xvar_loosen <- gg_md_loosen$topvars[!(gg_md_loosen$topvars %in% 
#                                            c('country', 'quarter'))]

# Partial dependence for loosening -----------
# Almost all variables
xvar_loosen <- gg_md_loosen$varselect$names %>% as.character
xvar_loosen <- xvar_loosen[!(xvar_loosen %in% c('country', 'quarter',
                                                  'executive_election_4qt',
                                                  'executive_election_4qt_after',
                                                  'ex_regime'))]

partial_bis_loosen <- plot.variable(rl1, xvar = xvar_loosen, partial = TRUE,
                                     show.plots = FALSE)

partial_loosen <- plot(gg_partial(partial_bis_loosen), panel = TRUE, 
                        alpha = 0.5) +
    geom_line() + xlab('') +
    scale_y_continuous(limits = c(0, 0.3)) +
    ylab('Predicted Probability of MPR Loosening\n') +
    xlab('\nPredictor Scale') +
    theme_bw()

ggsave(partial_loosen, filename = 'papers/figures/patial_loosen.pdf', 
       width = 10, height = 8)

# Interactions for loosening -----------
interaction_loosen <- find.interaction(rl1)

interact_loosen_plot <- plot(gg_interaction(interaction_loosen), 
                         panel = TRUE) + 
                        theme_bw() +
                        theme(axis.text.x = element_text(
                            angle = 90))

ggsave(interact_loosen_plot, 
       filename = 'papers/figures/interaction_check_loosen.pdf',
       width = 10, height = 8)
