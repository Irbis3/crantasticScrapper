
globalwd<-"/home/bst/student/hiparker/PracticalBatch"
## working directory of your files ##

fileloc<-"/home/bst/student/hiparker/ErrorRateProject/GSE2603/celfiles"
## where you have placed raw CEL files from GEO website

library(affy)
library(frma)
setwd(globalwd)
load("Section3_GSE2603.RData")

setwd(fileloc)
celA <- ReadAffy(filenames=array.files[batch=="A"], verbose=TRUE)
celB <- ReadAffy(filenames=array.files[batch=="B"], verbose=TRUE)

grp<-list(grpA,grpB)

setwd(globalwd)
source("sepdata.R")

set.seed(52085)

sepA<-list()
sepB<-list()

for(z in 1:100){

	## Data is separated into build, test groups
	sepA[[z]] <- sepdata(grp[[1]])
	sepB[[z]] <- sepdata(grp[[2]])
}

frmaA <- exprs(frma(celA))
frmaB <- exprs(frma(celB))


save(list=c("celA","celB","sepA","sepB","grp"),file="Section5_GSE2603_rma_sepdata.RData")
save(list=c("frmaA","frmaB","sepA","sepB","grp"),file="Section5_GSE2603_frma_sepdata.RData")

