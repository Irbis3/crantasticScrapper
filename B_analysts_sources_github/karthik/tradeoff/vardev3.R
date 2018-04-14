# vardev.R
source('initialize3.R')
num.cores <- 48
t3_vd <- mclapply(basic_params, do_vd_tradeoff, mc.cores= num.cores, mc.preschedule=TRUE)
fname2 <- generate_filename("../results_td3/t3_vd")
save(t3_vd, file = fname2)


# t3_vd <- llply(basic_params, do_vd_tradeoff, .progress = 'text')

# working_sims <- ldply(t3_vd, function(x) {
# 	if(!is.null(x$data)) {
# 			if(x$data[10,3]>x$data[20,3]) {
# 			  return(data.frame(x$params))
# 			}
# 		}
# 	})