
globalwd<-"/home/bst/student/hiparker/PracticalBatch"
## working directory of your files ##

sepdata <- "Section5_GSE2603_rma_sepdata.RData"
badpsfile <- "Section6_GSE2603_RMA_badps.RData"

setwd(globalwd)
load(sepdata)
load(badpsfile)
source("Section6_conf_fun_reduced.R")

conf_fun_reduced(strt=1,fin=100,
			  bad.ps.obj=rma.bad.ps.3,
				 datA=celA,
				 datB=celB,
				 filename="Section6_GSE2603_RMA_PAM_30",
				 TSP=FALSE,
				 PAM=TRUE,
				 RMA=TRUE)