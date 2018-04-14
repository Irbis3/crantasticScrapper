
globalwd<-"/home/bst/student/hiparker/PracticalBatch"
## working directory of your files ##

sepdata <- "Section5_GSE2034_rma_sepdata.RData"

setwd(globalwd)
load(sepdata)
source("Section5_conf_fun.R")

### CAN ONLY HAVE TSP OR PAM TRUE, NOT BOTH!! ###
fun(strt=1,
	fin=100,
	datA=celA, 
	datB=celB,
    direct=globalwd,
	filename="Section5_GSE2034_RMA_TSP",
	TSP=TRUE,
	PAM=FALSE,
	RMA=TRUE)
