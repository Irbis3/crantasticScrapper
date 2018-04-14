
globalwd<-"/home/bst/student/hiparker/PracticalBatch"
## working directory of your files ##

sepdata <- "Section5_GSE2603_frma_sepdata.RData"
badpsfile <- "Section6_GSE2603_fRMA_badps.RData"

setwd(globalwd)
load(sepdata)
load(badpsfile)
source("Section6_conf_fun_reduced.R")

conf_fun_reduced(strt=1,fin=100,
			  bad.ps.obj=frma.bad.ps.3,
				 datA=frmaA,
				 datB=frmaB,
				 filename="Section6_GSE2603_fRMA_TSP_30",
				 TSP=TRUE,
				 PAM=FALSE,
				 RMA=FALSE)