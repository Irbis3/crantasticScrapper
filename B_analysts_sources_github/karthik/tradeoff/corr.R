# corr.R
source('initialize.R')
# The full suite takes far too long on my dual-core mac so I run this on a cluster. The actual .pbs batch file is not part of this archive since it contains private account information.
total_cores <- 96
t1_corr <- mclapply(corr_params , do_vd_tradeoff, mc.cores = total_cores, mc.preschedule = TRUE)
# t1_corr <- llply(corr_params , do_vd_tradeoff, .progres = 'text')

# This generates a filename with a timestamp so I don't overwrite previous results
fname <- generate_filename("../results/t1_corr")
# Save toa  results folder
save(t1_corr, file = fname)

# with 60 cores
# 3600 corr params took 5.37 minutes on the cluster.
#      user    system   elapsed 
# 54660.235   322.604  1543.220 