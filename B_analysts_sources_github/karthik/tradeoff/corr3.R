# corr.R
source('initialize3.R')
# load('params/corr_params3.rda')
total_cores <- 72
t3_corr <- mclapply(corr_params , do_vd_tradeoff, mc.cores = total_cores, mc.preschedule = TRUE)
fname4 <- generate_filename("../results_td3/t3_corr")
save(t3_corr, file = fname4)
 