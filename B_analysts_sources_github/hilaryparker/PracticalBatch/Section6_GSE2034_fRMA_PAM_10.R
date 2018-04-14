
globalwd<-"/home/bst/student/hiparker/PracticalBatch"
## working directory of your files ##

sepdata <- "Section5_GSE2034_frma_sepdata.RData"
badpsfile <- "Section6_GSE2034_fRMA_badps.RData"

setwd(globalwd)
load(sepdata)
load(badpsfile)
source("Section6_conf_fun_reduced.R")

conf_fun_reduced(strt=1,fin=100,
			  bad.ps.obj=frma.bad.ps.1,
				 datA=frmaA,
				 datB=frmaB,
				 filename="Section6_GSE2034_fRMA_PAM_10",
				 TSP=FALSE,
				 PAM=TRUE,
				 RMA=FALSE)