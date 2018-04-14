
globalwd<-"/home/bst/student/hiparker/PracticalBatch"
## working directory of your files ##

sepdata <- "Section5_GSE2603_frma_sepdata.RData"

setwd(globalwd)
load(sepdata)
source("Section5_conf_fun.R")

### CAN ONLY HAVE TSP OR PAM TRUE, NOT BOTH!! ###
fun(strt=1,
	fin=100,
	datA=frmaA, 
	datB=frmaB,
    direct=globalwd,
	filename="Section5_GSE2603_fRMA_TSP",
	TSP=TRUE,
	PAM=FALSE,
	RMA=FALSE)
