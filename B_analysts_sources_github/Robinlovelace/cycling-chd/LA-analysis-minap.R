##########################################
######## Local Authority Analyses ########
##########################################


# Libraries
library(plyr)
library(data.table)
library(INLA)

# Load data

# la total pop results
la_commute_all <- readr::read_csv("data/11777054.csv", skip = 6)
la_commute_all = la_commute_all[grepl(pattern = "E", la_commute_all$mnemonic),]
sum(la_commute_all$`CS1190001 ALL PEOPLE : ALL PEOPLE`) # 22.4 million commuting
load("data/pop_10_13.RData")
sum(pop_10_13$population) # double counted
pop_11_filtered = filter(pop_10_13, age_band != "0-15", year == 2011, !grepl(pattern = "W", msoa_code))
sum(pop_11_filtered$population)
lkup <- readr::read_csv("data/la_msoa_lkup.csv") # Load LA to MSOA lookup
pop_11_la <- join(pop_11_filtered, lkup, by = "msoa_code", type = "right", match = "all") # Join together
pop_11_la = pop_11_la %>% group_by(la_code) %>% summarise(pop = sum(population))
mean(pop_11_la$pop)

la_minap <- readRDS("data/las_observed_expected_counts.Rds") # Minap data
la_transport <- readr::read_csv("data/la_transport.csv") # Exposures (totals)
la_transp_sex_age <- readr::read_csv("data/la_commuting_data_age_sex_2011.csv") # Exposures (by age and sex)
la_confs <- readr::read_csv("data/phe_la_data.csv") # Confounders

# Drop individuals aged 75+
la_minap <- la_minap[la_minap$age_band != "75+"]
la_minap <- la_minap[la_minap$age_band != "65-74"]

# Join into single file
hold <- join(la_minap, la_transport, by = "la_code", type = "left", match = "all")
hold2 <- join(hold, la_transp_sex_age, by = "la_code", type = "left", match = "all")
la_data <- join(hold2, la_confs, by = "la_code", type = "left", match = "all")
rm(hold, hold2, la_minap, la_transport, la_transp_sex_age, la_confs)
gc()

# Variables that are not obvious:
# expt_adms <- expected number of admissions
# pcwalk_11 <- percent who walk total all persons from 2011 Census (plus other variables with similar names but vary by mode of transport)
# pcp_16p_walk <- series of variables identified by three parts to name: (1) "pcp" is 'percent persons' - can be m (male) or f (female), (2) "16p" indicates age group so this is
# 16 plus, then there are 5 year age bands so 1619 is 16-19, (3) "walk" - mode of transport (obvious!)
# imd_15 <- IMD average score
# dm_10_11 <- Prevalence of diabetes (2010-11)
# pcsmoke_12 <- Percentage of adults who smoke (number is year - 2012 here)
# excess_wt_12_14 <- Percentage of adults with excess body weight (overweight or obese) (2012-14)
# pc_pa_12 <- Percentage of physicaly active adults
# dm_10_11 <- diabetes prevalence (2010/11)


### Analysing total admissions ###

la_data$pcp_1664_active <- la_data$pcp_1664_walk + la_data$pcp_1664_cycle

# Aggregate data
dt <- data.table(la_data)
la_persons <- dt[, list(admissions = sum(admissions, na.rm = TRUE), expt_adms = sum(expt_adms, na.rm = TRUE),
                        pccycle_11 = max(pcp_1664_cycle, na.rm = TRUE), pcwalk_11 = max(pcp_1664_walk, na.rm = TRUE), pcactive_11 = max(pcp_1664_active, na.rm = TRUE),
                        imd_15 = max(imd_2015, na.rm = TRUE), pcsmoke_12 = max(pcsmoke_12, na.rm = TRUE), pc_pa_12 = max(pc_pa_12, na.rm = TRUE),
                        excess_wt_12_14 = max(excess_wt_12_14, na.rm = TRUE), dm_10_11 = max(dm_10_11, na.rm = TRUE)),
                   by = c("la_code")] # NB I have used max for covariates since are all same values - just need to select one!
la_persons$imd_15[is.infinite(la_persons$imd_15)] <- NA # Set 'infinite' values as missing
la_persons$pcsmoke_12[is.infinite(la_persons$pcsmoke_12)] <- NA
la_persons$pc_pa_12[is.infinite(la_persons$pc_pa_12)] <- NA
la_persons$excess_wt_12_14[is.infinite(la_persons$excess_wt_12_14)] <- NA
la_persons$dm_10_11[is.infinite(la_persons$dm_10_11)] <- NA
la_persons$pcactive_11[is.infinite(la_persons$pcactive_11)] <- NA
la_persons$pcwalk_11[is.infinite(la_persons$pcwalk_11)] <- NA
la_persons$pccycle_11[is.infinite(la_persons$pccycle_11)] <- NA

# Regression - active transport (unadjusted)
formula <- admissions ~ 1 + pcactive_11
model_p1 <- inla(formula, family = "nbinomial", data = la_persons, offset = log(expt_adms), control.compute=list(dic=T))
exp(model_p1$summary.fixed)

# Regression - active transport (adjusted)
formula <- admissions ~ 1 + pcactive_11 + imd_15 + pcsmoke_12 + excess_wt_12_14 + pc_pa_12
model_p2 <- inla(formula, family = "nbinomial", data = la_persons, offset = log(expt_adms), control.compute=list(dic=T))
exp(model_p2$summary.fixed)

# Regression (unadjusted)
formula <- admissions ~ 1 + pccycle_11 + pcwalk_11
model_p3 <- inla(formula, family = "nbinomial", data = la_persons, offset = log(expt_adms), control.compute=list(dic=T))
exp(model_p3$summary.fixed)

# Regression (adjusted)
formula <- admissions ~ 1 + pccycle_11 + pcwalk_11 + imd_15 + pcsmoke_12 + excess_wt_12_14 + pc_pa_12
model_p4 <- inla(formula, family = "nbinomial", data = la_persons, offset = log(expt_adms), control.compute=list(dic=T))
exp(model_p4$summary.fixed)

# generate summary table
ff_res = exp(model_p$summary.fixed)
ff_res_cywalk = ff_res[2:3, c("mean", "0.025quant", "0.975quant")]
#dir.create("la_results")
write.csv(ff_res, "la_results/ff_res.csv")
write.csv(ff_res_cywalk, "la_results/ff_res_cywalk.csv")

(ff_res - 1) * 100
rm(la_persons)
names(la_data)


### Analysing admissions by sex ###

# Aggregate data
dt <- data.table(la_data)
la_sex <- dt[, list(admissions = sum(admissions, na.rm = TRUE), expt_adms = sum(expt_adms, na.rm = TRUE),
                        pccycle_fm_11 = max(pcf_1674_cycle, na.rm = TRUE), pcwalk_fm_11 = max(pcf_1674_walk, na.rm = TRUE),
                        pccycle_ma_11 = max(pcm_1674_cycle, na.rm = TRUE), pcwalk_ma_11 = max(pcm_1674_walk, na.rm = TRUE),
                        imd_15 = max(imd_2015, na.rm = TRUE), pcsmoke_12 = max(pcsmoke_12, na.rm = TRUE), pc_pa_12 = max(pc_pa_12, na.rm = TRUE),
                        excess_wt_12_14 = max(excess_wt_12_14, na.rm = TRUE), dm_10_11 = max(dm_10_11, na.rm = TRUE)),
                 by = c("sex", "la_code")] # NB I have used max for covariates since are all same values - just need to select one!
la_sex$imd_15[is.infinite(la_sex$imd_15)] <- NA # Set 'infinite' values as missing
la_sex$pcsmoke_12[is.infinite(la_sex$pcsmoke_12)] <- NA
la_sex$pc_pa_12[is.infinite(la_sex$pc_pa_12)] <- NA
la_sex$excess_wt_12_14[is.infinite(la_sex$excess_wt_12_14)] <- NA
la_sex$dm_10_11[is.infinite(la_sex$dm_10_11)] <- NA
la_sex$pccycle_fm_11[is.infinite(la_sex$pccycle_fm_11)] <- NA
la_sex$pcwalk_fm_11[is.infinite(la_sex$pcwalk_fm_11)] <- NA
la_sex$pccycle_ma_11[is.infinite(la_sex$pccycle_ma_11)] <- NA
la_sex$pcwalk_ma_11[is.infinite(la_sex$pcwalk_ma_11)] <- NA

la_males <- la_sex[la_sex$sex=="Male"]
la_females <- la_sex[la_sex$sex=="Female"]
rm(la_sex)
gc()

# Regression

# Males (unadjusted)
formula <- admissions ~ 1 + pccycle_ma_11 + pcwalk_ma_11
model_m1 <- inla(formula, family = "nbinomial", data = la_males, offset = log(expt_adms), control.compute=list(dic=T))
exp(model_m1$summary.fixed)

# Males (adjusted)
formula <- admissions ~ 1 + pccycle_ma_11 + pcwalk_ma_11 + imd_15 + pcsmoke_12 + excess_wt_12_14 + pc_pa_12
model_m2 <- inla(formula, family = "nbinomial", data = la_males, offset = log(expt_adms), control.compute=list(dic=T))
exp(model_m2$summary.fixed)

# Females (unadjusted)
formula <- admissions ~ 1 + pccycle_fm_11 + pcwalk_fm_11
model_f1 <- inla(formula, family = "nbinomial", data = la_females, offset = log(expt_adms), control.compute=list(dic=T))
exp(model_f1$summary.fixed)

# Females (adjusted)
formula <- admissions ~ 1 + pccycle_fm_11 + pcwalk_fm_11 + imd_15 + pcsmoke_12 + excess_wt_12_14 + pc_pa_12
model_f2 <- inla(formula, family = "nbinomial", data = la_females, offset = log(expt_adms), control.compute=list(dic=T))
exp(model_f2$summary.fixed)

rm(la_males, la_females, dt)
gc()



### Analysing admissions by age and sex ###


# Aggregate data
dt <- data.table(la_data)
la_sex <- dt[, list(admissions = sum(admissions, na.rm = TRUE), expt_adms = sum(expt_adms, na.rm = TRUE),
                    pcf_1624_walk = max(pcf_1624_walk, na.rm = TRUE), pcf_1624_cycle = max(pcf_1624_cycle, na.rm = TRUE),
                    pcf_2534_walk = max(pcf_2534_walk, na.rm = TRUE), pcf_2534_cycle = max(pcf_2534_cycle, na.rm = TRUE),
                    pcf_3544_walk = max(pcf_3544_walk, na.rm = TRUE), pcf_3544_cycle = max(pcf_3544_cycle, na.rm = TRUE),
                    pcf_4554_walk = max(pcf_4554_walk, na.rm = TRUE), pcf_4554_cycle = max(pcf_4554_cycle, na.rm = TRUE),
                    pcf_5564_walk = max(pcf_5564_walk, na.rm = TRUE), pcf_5564_cycle = max(pcf_5564_cycle, na.rm = TRUE),
                    pcf_65p_walk = max(pcf_65p_walk, na.rm = TRUE), pcf_65p_cycle = max(pcf_65p_cycle, na.rm = TRUE),
                    pcm_1624_walk = max(pcm_1624_walk, na.rm = TRUE), pcm_1624_cycle = max(pcm_1624_cycle, na.rm = TRUE),
                    pcm_2534_walk = max(pcm_2534_walk, na.rm = TRUE), pcm_2534_cycle = max(pcm_2534_cycle, na.rm = TRUE),
                    pcm_3544_walk = max(pcm_3544_walk, na.rm = TRUE), pcm_3544_cycle = max(pcm_3544_cycle, na.rm = TRUE),
                    pcm_4554_walk = max(pcm_4554_walk, na.rm = TRUE), pcm_4554_cycle = max(pcm_4554_cycle, na.rm = TRUE),
                    pcm_5564_walk = max(pcm_5564_walk, na.rm = TRUE), pcm_5564_cycle = max(pcm_5564_cycle, na.rm = TRUE),
                    pcm_65p_walk = max(pcm_65p_walk, na.rm = TRUE), pcm_65p_cycle = max(pcm_65p_cycle, na.rm = TRUE),
                    imd_15 = max(imd_2015, na.rm = TRUE), pcsmoke_12 = max(pcsmoke_12, na.rm = TRUE), pc_pa_12 = max(pc_pa_12, na.rm = TRUE),
                    excess_wt_12_14 = max(excess_wt_12_14, na.rm = TRUE), dm_10_11 = max(dm_10_11, na.rm = TRUE)),
             by = c("la_code", "sex", "age_band")] # NB I have used max for covariates since are all same values - just need to select one!

invisible(lapply(names(la_sex),function(.name) set(la_sex, which(is.infinite(la_sex[[.name]])), j = .name,value =NA))) # Set 'infinite' values as missing

la_males <- la_sex[la_sex$sex=="Male"]
la_females <- la_sex[la_sex$sex=="Female"]
rm(la_sex)
gc()

age_results = data.frame(matrix(nrow = 10, ncol = 8))
names(age_results) = c("Age band", "Explanatory variable", "IRR",	"Lower CI",	"Upper CI",	"IRR",	"Lower CI",	"Upper CI")
age_results$`Age band` = rep(c("16-24", "25-34", "35-44", "45-54", "55-64"), each = 2)

age_results$`Explanatory variable` = rep(c("% Cycle", "% Walk"), length.out = nrow(age_results))

# Males #
hold <- la_males[la_males$age_band == "16-24"]
formula <- admissions ~ 1 + pcm_1624_cycle + pcm_1624_walk + imd_15 + pcsmoke_12 + excess_wt_12_14 + pc_pa_12
model_m <- inla(formula, family = "nbinomial", data = hold, control.compute=list(dic=T))
summary(model_m)
age_results[1:2, 3:5] = exp(model_m$summary.fixed[2:3, c("mean", "0.025quant", "0.975quant")])

hold <- la_males[la_males$age_band == "25-34"]
formula <- admissions ~ 1 + pcm_2534_cycle + pcm_2534_walk + imd_15 + pcsmoke_12 + excess_wt_12_14 + pc_pa_12
model_m <- inla(formula, family = "nbinomial", data = hold, control.compute=list(dic=T))
summary(model_m)
age_results[3:4, 3:5] = exp(model_m$summary.fixed[2:3, c("mean", "0.025quant", "0.975quant")])

hold <- la_males[la_males$age_band == "35-44"]
formula <- admissions ~ 1 + pcm_3544_cycle + pcm_3544_walk + imd_15 + pcsmoke_12 + excess_wt_12_14 + pc_pa_12
model_m <- inla(formula, family = "nbinomial", data = hold, control.compute=list(dic=T))
summary(model_m)
age_results[5:6, 3:5] = exp(model_m$summary.fixed[2:3, c("mean", "0.025quant", "0.975quant")])

hold <- la_males[la_males$age_band == "45-54"]
formula <- admissions ~ 1 + pcm_4554_cycle + pcm_4554_walk + imd_15 + pcsmoke_12 + excess_wt_12_14 + pc_pa_12
model_m <- inla(formula, family = "nbinomial", data = hold, control.compute=list(dic=T))
summary(model_m)
age_results[7:8, 3:5] = exp(model_m$summary.fixed[2:3, c("mean", "0.025quant", "0.975quant")])

hold <- la_males[la_males$age_band == "55-64"]
formula <- admissions ~ 1 + pcm_5564_cycle + pcm_5564_walk + imd_15 + pcsmoke_12 + excess_wt_12_14 + pc_pa_12
model_m <- inla(formula, family = "nbinomial", data = hold, control.compute=list(dic=T))
summary(model_m)
age_results[9:10, 3:5] = exp(model_m$summary.fixed[2:3, c("mean", "0.025quant", "0.975quant")])

rm(model_m)

# Females #
hold <- la_females[la_females$age_band == "16-24"]
formula <- admissions ~ 1 + pcf_1624_cycle + pcf_1624_walk + imd_15 + pcsmoke_12 + excess_wt_12_14 + pc_pa_12
model_f <- inla(formula, family = "nbinomial", data = hold, control.compute=list(dic=T))
summary(model_f)
age_results[1:2, 6:8] = exp(model_f$summary.fixed[2:3, c("mean", "0.025quant", "0.975quant")])

hold <- la_females[la_females$age_band == "25-34"]
formula <- admissions ~ 1 + pcf_2534_cycle + pcf_2534_walk + imd_15 + pcsmoke_12 + excess_wt_12_14 + pc_pa_12
model_f <- inla(formula, family = "nbinomial", data = hold, control.compute=list(dic=T))
summary(model_f)
age_results[3:4, 6:8] = exp(model_f$summary.fixed[2:3, c("mean", "0.025quant", "0.975quant")])

hold <- la_females[la_females$age_band == "35-44"]
formula <- admissions ~ 1 + pcf_3544_cycle + pcf_3544_walk + imd_15 + pcsmoke_12 + excess_wt_12_14 + pc_pa_12
model_f <- inla(formula, family = "nbinomial", data = hold, control.compute=list(dic=T))
summary(model_f)
age_results[5:6, 6:8] = exp(model_f$summary.fixed[2:3, c("mean", "0.025quant", "0.975quant")])

hold <- la_females[la_females$age_band == "45-54"]
formula <- admissions ~ 1 + pcf_4554_cycle + pcf_4554_walk + imd_15 + pcsmoke_12 + excess_wt_12_14 + pc_pa_12
model_f <- inla(formula, family = "nbinomial", data = hold, control.compute=list(dic=T))
summary(model_f)
age_results[7:8, 6:8] = exp(model_f$summary.fixed[2:3, c("mean", "0.025quant", "0.975quant")])

hold <- la_females[la_females$age_band == "55-64"]
formula <- admissions ~ 1 + pcf_5564_cycle + pcf_5564_walk + imd_15 + pcsmoke_12 + excess_wt_12_14 + pc_pa_12
model_f <- inla(formula, family = "nbinomial", data = hold, control.compute=list(dic=T))
summary(model_f)
age_results[9:10, 6:8] = exp(model_f$summary.fixed[2:3, c("mean", "0.025quant", "0.975quant")])

age_results
write.csv(age_results, "la_results/age_results.csv")

rm(model_f)


### Lag effect analysis ###


# Add lag effect data
la_2001 <- readRDS("data/las_exposures_2001.Rds")
la_2001[3:32] <- la_2001[3:32] * 100 # Multiply by 100

age_results = data.frame(matrix(nrow = 8, ncol = 8))
names(age_results) = c("Age band", "Explanatory variable", "IRR",  "Lower CI",	"Upper CI",	"IRR",	"Lower CI",	"Upper CI")
age_results$`Age band` = rep(c("25-34", "35-44", "45-54", "55-64"), each = 2)

age_results$`Explanatory variable` = rep(c("% Cycle lag", "% Walk lag"), length.out = nrow(age_results))

# Males #
hold <- la_males[la_males$age_band == "25-34"]
hold <- join(hold, la_2001, by = "la_code", type = "left", match = "all")
formula <- admissions ~ 1 + pcm01_1624_cycle + pcm01_1624_walk + imd_15 + pcsmoke_12 + excess_wt_12_14 + pc_pa_12
model_m <- inla(formula, family = "nbinomial", data = hold, control.compute=list(dic=T))
summary(model_m)
age_results[1:2, 3:5] = exp(model_m$summary.fixed[2:3, c("mean", "0.025quant", "0.975quant")])

hold <- la_males[la_males$age_band == "35-44"]
hold <- join(hold, la_2001, by = "la_code", type = "left", match = "all")
formula <- admissions ~ 1 + pcm01_2534_cycle + pcm01_2534_walk + imd_15 + pcsmoke_12 + excess_wt_12_14 + pc_pa_12
model_m <- inla(formula, family = "nbinomial", data = hold, control.compute=list(dic=T))
summary(model_m)
age_results[3:4, 3:5] = exp(model_m$summary.fixed[2:3, c("mean", "0.025quant", "0.975quant")])

hold <- la_males[la_males$age_band == "45-54"]
hold <- join(hold, la_2001, by = "la_code", type = "left", match = "all")
formula <- admissions ~ 1 + pcm01_3544_cycle + pcm01_3544_walk + imd_15 + pcsmoke_12 + excess_wt_12_14 + pc_pa_12
model_m <- inla(formula, family = "nbinomial", data = hold, control.compute=list(dic=T))
summary(model_m)
age_results[5:6, 3:5] = exp(model_m$summary.fixed[2:3, c("mean", "0.025quant", "0.975quant")])

hold <- la_males[la_males$age_band == "55-64"]
hold <- join(hold, la_2001, by = "la_code", type = "left", match = "all")
formula <- admissions ~ 1 + pcm01_4554_cycle + pcm01_4554_walk + imd_15 + pcsmoke_12 + excess_wt_12_14 + pc_pa_12
model_m <- inla(formula, family = "nbinomial", data = hold, control.compute=list(dic=T))
summary(model_m)
age_results[7:8, 3:5] = exp(model_m$summary.fixed[2:3, c("mean", "0.025quant", "0.975quant")])

rm(model_m)

# Females #
hold <- la_females[la_females$age_band == "25-34"]
hold <- join(hold, la_2001, by = "la_code", type = "left", match = "all")
formula <- admissions ~ 1 + pcf01_1624_cycle + pcf01_1624_walk + imd_15 + pcsmoke_12 + excess_wt_12_14 + pc_pa_12
model_f <- inla(formula, family = "nbinomial", data = hold, control.compute=list(dic=T))
summary(model_f)
age_results[1:2, 6:8] = exp(model_f$summary.fixed[2:3, c("mean", "0.025quant", "0.975quant")])

hold <- la_females[la_females$age_band == "35-44"]
hold <- join(hold, la_2001, by = "la_code", type = "left", match = "all")
formula <- admissions ~ 1 + pcf01_2534_cycle + pcf01_2534_walk + imd_15 + pcsmoke_12 + excess_wt_12_14 + pc_pa_12
model_f <- inla(formula, family = "nbinomial", data = hold, control.compute=list(dic=T))
summary(model_f)
age_results[3:4, 6:8] = exp(model_f$summary.fixed[2:3, c("mean", "0.025quant", "0.975quant")])

hold <- la_females[la_females$age_band == "45-54"]
hold <- join(hold, la_2001, by = "la_code", type = "left", match = "all")
formula <- admissions ~ 1 + pcf01_3544_cycle + pcf01_3544_walk + imd_15 + pcsmoke_12 + excess_wt_12_14 + pc_pa_12
model_f <- inla(formula, family = "nbinomial", data = hold, control.compute=list(dic=T))
summary(model_f)
age_results[5:6, 6:8] = exp(model_f$summary.fixed[2:3, c("mean", "0.025quant", "0.975quant")])

hold <- la_females[la_females$age_band == "55-64"]
hold <- join(hold, la_2001, by = "la_code", type = "left", match = "all")
formula <- admissions ~ 1 + pcf01_4554_cycle + pcf01_4554_walk + imd_15 + pcsmoke_12 + excess_wt_12_14 + pc_pa_12
model_f <- inla(formula, family = "nbinomial", data = hold, control.compute=list(dic=T))
summary(model_f)
age_results[7:8, 6:8] = exp(model_f$summary.fixed[2:3, c("mean", "0.025quant", "0.975quant")])

age_results
write.csv(age_results, "la_results/age_results_lag.csv")

rm(model_f)

