
globalwd<-"/home/bst/student/hiparker/PracticalBatch"
## working directory of your files ##

sepdata <- "Section5_GSE2034_rma_sepdata.RData"
badpsfile <- "Section6_GSE2034_RMA_badps.RData"

setwd(globalwd)
load(sepdata)
load(badpsfile)
source("Section6_conf_fun_reduced.R")

conf_fun_reduced(strt=1,fin=100,
			  bad.ps.obj=rma.bad.ps.4,
				 datA=celA,
				 datB=celB,
				 filename="Section6_GSE2034_RMA_TSP_40",
				 TSP=TRUE,
				 PAM=FALSE,
				 RMA=TRUE)