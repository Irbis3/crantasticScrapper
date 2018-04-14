### Results ###

# Libraries


# 2001 Exposure
res_f_01_un <- readRDS("la_results/yearly_results_f_unadj.Rds") # Females unadjusted
res_f_01_un

res_f_01_ad <- readRDS("./la_results/yearly_results_f_unadj.Rds") # Females adjusted
res_f_01_ad

res_m_01_un <- readRDS("./la_results/yearly_results_m_unadj.Rds") # Males unadjusted
res_m_01_un

res_m_01_ad <- readRDS("./la_results/yearly_results_m_adj.Rds") # Males adjusted
res_m_01_ad

# 2011 Exposure
res_f_11_un <- readRDS("./la_results/la_unadj_f_2011_2013.Rds") # Females unadjusted
res_f_11_un

res_f_11_ad <- readRDS("./la_results/la_adj_f_2011_2013.Rds") # Females adjusted
res_f_11_ad

res_m_11_un <- readRDS("./la_results/la_unadj_m_2011_2013.Rds") # Males unadjusted
res_m_11_un

res_m_11_ad <- readRDS("./la_results/la_adj_m_2011_2013.Rds") # Males adjusted
res_m_11_ad

# Tables that work
yearly_results_f <- readRDS("~/paper-repos/cycling-chd/la_results/yearly_results_f.Rds") # doesn't tell us much
las_observed_expected = readRDS("data/las_observed_expected_counts.Rds")

# maps
devtools::install_github("robinlovelace/ukboundaries")
library(sf)
library(tmap)
library(dplyr)
library(ukboundaries)
summary(sel <- lad2011_simple$code %in% las_observed_expected$la_code)
plot(lad2011_simple$geometry[sel])
las_res = las_observed_expected %>%
  group_by(la_code) %>%
  summarise(total = sum(population), Observed = sum(admissions), Expected = sum(expt_adms)) %>%
  rename(code = la_code) %>%
  mutate(Observed = Observed / total * 1e4 / 11, Expected = Expected / total * 1e4 / 11)

cor(las_res$Observed, las_res$Expected)
lads = left_join(lad2011_simple, las_res)

tm_shape(lads) +
  tm_fill(c("Observed", "Expected"), breaks = 0:4)

las_exposures = readRDS("data/las_exposures_2001.Rds")
names_exp_plot = names(las_exposures)[-(1:2)][1:10]
lads_exp = left_join(lad2011_simple, las_exposures, by = c("code" = "la_code"))
qtm(lads_exp, names_exp_plot)
