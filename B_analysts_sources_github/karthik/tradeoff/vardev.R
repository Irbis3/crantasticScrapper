# vardev.R
source('initialize.R')
t1_vd <- mclapply(basic_params, do_vd_tradeoff, mc.cores = 12, mc.preschedule = TRUE)
# t1_vd <- llply(basic_params, do_vd_tradeoff, .progress = 'text')

fname2 <- generate_filename("../results/t1_vd")
save(t1_vd, file=fname2)

# vd params took 0.21 minutes
# user  system elapsed 
# 124.623   1.300  18.342


