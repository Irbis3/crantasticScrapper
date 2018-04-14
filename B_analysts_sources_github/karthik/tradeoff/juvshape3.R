# juvshape.R
source('initialize3.R')
total_cores <- 72
t3_juvshape <- mclapply(vd_params, do_vd_tradeoff, mc.cores = total_cores, mc.preschedule = TRUE)
fname3 <- generate_filename("../results_td3/t3_juvshape")
save(t3_juvshape, file = fname3)
 