################################################################################
################################################################################
##                              Section 3                                     ##
################################################################################
################################################################################


globalwd<-"/home/bst/student/hiparker/PracticalBatch"
## working directory of your files ##

fileloc<-"/home/bst/student/hiparker/ErrorRateProject/GSE2034/celfiles"
## where you have placed raw CEL files from GEO website

setwd(globalwd)
library(wang05)
data(wang05)
# wang05 package can be obtained from
# http://pierotti.group.ifom-ieo-campus.it/biocdb/data/experiment/

arrays <- rownames(pData(wang05))
len <- length(arrays)
files <- dir(fileloc)

array.ind <- rep(0,len)
for(i in 1:len){
    x <- arrays[i]
    array.ind[i] <- grep(x, files, ignore.case=TRUE)
}
array.files <- files[array.ind]

pheno <- pData(wang05)


# function for extracting cel file date #
source("celfileDate.R")

setwd(fileloc)
dts <- rep(NA,len)
for(i in 1:len){
    dts[i] <- celfileDate(array.files[i])
}
dts<-dts-min(dts)+1

# cutoffs based on histogram #
sum(dts<100)
#[1] 102
sum(dts>100 & dts<300)
#[1] 87
sum(dts>300)
#[1] 97


batch <- rep(0,len)
batch[dts<100]<-"A"
batch[100<dts & dts<300]<-"B"
batch[300<dts]<-"C"

grp<-rep(0,len)
grp[pheno$ER.Status=="ER+"]<-1

grpA <- grp[batch=="A"]
sum(grpA)
#[1] 68
grpB <- grp[batch=="B"]
sum(grpB)
#[1] 64
grpC <- grp[batch=="C"]
sum(grpC)
#[1] 77

setwd(globalwd)
save.image(file="Section3_GSE2034.RData")
