# Initialize basic libraries and functions
# ------------------------------------------------------

# loading libraries
rm(list = ls())
# setwd('~/Github/postdoc/tradeoff/td3/analysis_td3')
message("Loading libraries \n")
library(plyr)
library(varDev2)
suppressPackageStartupMessages(library(lubridate))
library(stringr)
library(multicore)
suppressPackageStartupMessages(library(data.table))
suppressPackageStartupMessages(library(R.utils))
message("Loading functions \n")
source("tfunctions3.R")
load('working_sims.rdata')
# ------------------------------------------------------
#  This is for the tradeoff between juvenile growth and fecundity.
# ------------------------------------------------------

message("Generating various tradeoff combinations \n")
# Basic tradeoff, adult survival, and fecundity
a <- seq(0.9, 0.6, by = -0.1)
b <- seq(-0.9, -0.6, by = 0.1)
sA <- seq(0.6, 0.9, by = 0.1)
sJ <- seq(0.6, 0.9, by = 0.1)
basic_params <- param_combs(a, b, sA, sJ)
# 25

# Since not all combinations result in a tradeoff, we remove ones that dont to avoid computation overhead
basic_params2 <- lapply(basic_params, function(x) {
	 load('working_sims.rdata')
 	 if(x$sim_id %in% working_sims$sim_id) {
 	   return(x) 
 	   }
	})

basic_params <- compact(basic_params2)

# ------------------------------------------------------
message("\n juvparams")
# Next, add in a range of cv values
# cv is defined as 1/sqrt(juvshape)
jps <- function(x) return((1/x)*(1/x))
juvshape <- jps(seq(0.1, 1, by = 0.1))
vd_params <- param_combs_jg(a, b, sA, sJ, juvshape)
# length vd_params: 2560

message("\n Finally, corr_params")
# ------------------------------------------------------
# Now add in a correlation
# corr <- c(0.1, 0.25, 0.5, 0.75, .99)
corr <- seq(0.1, 0.9, by = 0.25)
corr_params <- param_combs_corr(a, b, sA, sJ, juvshape, corr)
# 10240
 