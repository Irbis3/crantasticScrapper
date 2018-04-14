# juvshape.R
source('initialize.R')
total_cores <- 60
t1_juvshape <- mclapply(vd_params, do_vd_tradeoff, mc.cores = total_cores, mc.preschedule = TRUE)
# Uncomment below (and comment out line above) for running this locally.
# t1_juvshape <- llply(vd_params, do_vd_tradeoff, .progress = 'text')

fname3 <- generate_filename("../results/t1_juv")
save(t1_juvshape, file=fname3)

# 3600 juvshape params took 1.38 minutes on the cluster.
#     user   system  elapsed 
# 5916.513   83.657  180.187 