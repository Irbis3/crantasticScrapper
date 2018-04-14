setwd('~/Github/postdoc/tradeoff/td3/analysis_td3')
source('initialize3.R')
source('tfunctions3.R')
library(data.table)

t1_simple <- llply(sample(basic_params,1), do_tradeoff, .progress = 'text')
t1_vd <- llply(sample(basic_params,1), do_vd_tradeoff, .progress = 'text')
t1_juvshape <- llply(sample(vd_params,1), do_vd_tradeoff, .progress = 'text')
t1_corr <- llply(sample(corr_params,1), do_vd_tradeoff, .progress = 'text')
