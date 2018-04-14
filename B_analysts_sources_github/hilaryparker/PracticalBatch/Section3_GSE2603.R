################################################################################
################################################################################
##                              Section 3                                     ##
################################################################################
################################################################################


globalwd<-"/home/bst/student/hiparker/PracticalBatch"
## working directory of your files ##

fileloc<-"/nexsan/bst2/microarray/CEL/GPL96/"
## where you have placed raw CEL files from GEO website

setwd(globalwd)
library(minn05)
data(minn05)
pheno <- pData(minn05)
pheno <- pheno[pheno$sample.type!="cell line",]

arrays <- rownames(pheno)
len <- length(arrays)
files <- dir(fileloc)


array.ind <-rep (0,len)

for(i in 1:len){
    x <- arrays[i]
    array.ind[i] <- grep(x, files, ignore.case=TRUE)
}
array.files <- files[array.ind]


# function for extracting cel file date #
source("celfileDate.R")


setwd(fileloc)
dts <- rep(NA,len)
for(i in 1:len){
    dts[i] <- celfileDate(array.files[i])
}
dts<-dts-min(dts)+1


sum(dts<200)
#[1] 27
sum(200<dts & dts<300)
#[1] 34
sum(dts>300)
#[1] 38


batch <- rep(0,len)
batch[dts<200] <- "C"
batch[200<dts & dts<300] <- "A"
batch[dts>300] <- "B"

table(pheno$Path.ER.status)

grp <- rep(0,len)
grp[pheno$Path.ER.status=="P"] <- 1

grpA <- grp[batch=="A"]
sum(grpA)
#[1] 15
grpB <- grp[batch=="B"]
sum(grpB)
#[1] 23
grpC <- grp[batch=="C"]
sum(grpC)
#[1] 19


setwd(globalwd)
save.image(file="Section3_GSE2603.RData")



