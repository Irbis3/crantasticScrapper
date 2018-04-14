# simple.R
source('initialize3.R')
num.cores <- 12
t3_simple <- mclapply(basic_params, do_tradeoff, mc.cores= num.cores, mc.preschedule=TRUE)
t3_simple <- llply(basic_params, do_tradeoff, .progress = 'text')
fname1 <- generate_filename("../results_td3/t3_simple")
save(t3_simple, file = fname1)
