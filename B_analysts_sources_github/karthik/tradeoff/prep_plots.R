rm(list=ls())
library(plyr)
suppressPackageStartupMessages(library(data.table))
setwd('~/Github/postdoc/tradeoff/td3/viz_td3')
source('../analysis_td3/tfunctions3.R')
load('../results_td3/t3_corr_2012-09-11_21_47_16.rdata')
load('../results_td3/t3_juvshape_2012-09-11_20_59_54.rdata')
load('../results_td3/t3_simple_2012-09-11_20_46_54.rdata')
load('../results_td3/t3_vd_2012-09-11_20_47_25.rdata')


xx <- ldply(t3_juvshape, function(x) class(x[[1]]))
keep <- which(xx$V1=="data.frame")
t3_juvshape <- t3_juvshape[keep]


# ------------------------------ SIMPLE
simple_max <- ldply(t3_simple, function(x) {
		amax <- arg_max(x$data)
		return(data.frame(a=x$params$a, b=x$params$b, sA=x$params$sA,  juvshape = NA, corr = NA, mstar = amax[[1]], amax_y = amax[[2]], sim_id = x$params$sim_id, type = "simple"))
	}, .progress = 'text')
simple_max$cv <- 1


# ------------------------------ VD
# VD

t3_vd <- llply(t3_vd, function(x) if(!is.null(x$data)) return(x))
vd_max <- ldply(t3_vd, function(x) {
		amax <- arg_max(x$data)
		return(data.frame(a=x$params$a, b=x$params$b, sA=x$params$sA, juvshape = NA, corr = NA, mstar = amax[[1]], amax_y = amax[[2]], sim_id = x$params$sim_id, type = "vd"))
	}, .progress = 'text')
vd_max$cv <- 1

# ------------------------------ JUVSHAPE

process_mstar_juvshape <- function(x) {
	if(!inherits(x, "try-error")) {
	 if(class(x$data)=="data.frame") {
		# message(sprintf("%s",x$params$sim_id))
		amax <- arg_max(x$data)
		cv <- unique(x$data$cv)
		return(data.frame(a=x$params$a, b=x$params$b, sA=x$params$sA, juvshape = x$params$juvshape, corr = NA,  mstar = amax[[1]], amax_y = amax[[2]], sim_id = x$params$sim_id,type = "juvshape", cv = cv))
		 }
		} else {
			return(data.frame(a= NA, b= NA, sA= NA, juvshape = NA, corr = NA,  mstar = "Nan", amax_y = "Nan", sim_id = x$params$sim_id, type = "juvshape", cv = NA))
		}
	}
# ------------------------------cleaning up juvshape.

remove_bad <- function(all) {
keep <- apply(all, 1, function(x) !any("Nan" %in% x))
cleaned_all <- all[keep, ]
return(cleaned_all)
}

juvshape_max <- ldply(t3_juvshape, process_mstar_juvshape, .progress = 'text')
juvshape_max <- remove_bad(juvshape_max)

# ------------------------------working through the mstar of corr.
process_mstar_cv <- function(x) {
	  if(class(x)!="try-error") {
		# message(sprintf("%s, %s", class(x$data), dim(x$data)[1]))
		if(class(x$data)=="data.frame") {
		# message(sprintf("%s",x$params$sim_id))
		amax <- arg_max(x$data)
		cv <- unique(x$data$cv)
		return(data.frame(a=x$params$a, b=x$params$b, sA=x$params$sA,  juvshape = x$params$juvshape, corr = x$params$corr,  mstar = amax[[1]], amax_y = amax[[2]], sim_id = x$params$sim_id, type = "corr", cv = cv))
	}
		else {
			return(data.frame(a = NA, b = NA, sA = NA, juvshape = NA, corr = NA,  mstar = "Nan", amax_y = "Nan", sim_id = x$params$sim_id, type = "corr", cv = NA))
		}
	}
	}


# ------------------------------
# corr_data <- llply(t3_corr, function(x) class(x[1]))
# corr_data <- ldply(corr_data)
# index <-which(corr_data$V1=="character")
# t3_corr2 <- t3_corr[-index]
# t3_corr <- t3_corr2
# save(t3_corr, file="cleaned_t3_corr.rdata")
# Removing empty results from t3_corr
message("Calculating arg max for t3_corr \n")
x2 <- ldply(t3_corr, length)
if(length(which(x2$V1==0))>0) {
	t3_corr <- t3_corr[-which(x2$V1==0)] } else {
		t3_corr <- t3_corr
	}

corr_max <- ldply(t3_corr, process_mstar_cv, .progress = 'text')
corr_max <- remove_bad(corr_max)

all <- rbind(simple_max, vd_max, juvshape_max, corr_max)
message(sprintf("All currently has dim %s %s \n",dim(all)[1], dim(all)[2]))
# Remove any items that were not computed.
bad <- apply(all, 1, function(x) !any("Nan" %in% x))
cleaned_all <- all[bad, ]
message(sprintf("All currently has dim %s %s \n",dim(cleaned_all)[1], dim(cleaned_all)[2]))
cleaned_all <- data.table(all)
save(cleaned_all, file="all_results_new.rda")



# Saving individual results in separate data tables
working_corr <- data.table(ldply(t3_corr, function(x) data.frame(x[[2]]), .progress='text'))
working_js <- data.table(ldply(t3_juvshape, function(x) data.frame(x[[2]]), .progress = 'text'))
working_vd <- data.table(ldply(t3_vd, function(x) data.frame(x[[2]]), .progress = 'text'))
working_simple <- data.table(ldply(t3_simple, function(x) data.frame(x[[2]]), .progress = 'text'))

# Setting keys to make them sortable.
setkeyv(working_corr, "sim_id")
setkeyv(working_js, "sim_id")
setkeyv(working_vd, "sim_id")
setkeyv(working_simple, "sim_id")

# Saving the files to disk.
save(working_vd, file="working_vd.rdata")
save(working_corr, file="working_corr.rdata")
save(working_js, file="working_js.rdata")
save(working_simple, file="working_simple.rdata")

# Next run generate_all_plots.R to visualize these results.
# ------------------------------------------------------------------
