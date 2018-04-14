################################################################################
################################################################################
##                              Section 4.0                                   ##
################################################################################
################################################################################

globalwd<-"/home/bst/student/hiparker/PracticalBatch"
## working directory of your files ##

fileloc<-"/home/bst/student/hiparker/ErrorRateProject/GSE2034/celfiles"
## where you have placed raw CEL files from GEO website

setwd(globalwd)
library(affy)
library(tspair)
library(pamr)
library(lars)

load("Section3_GSE2034.RData")

setwd(fileloc)
celA <- ReadAffy(filenames=array.files[batch=="A"], verbose=TRUE)
celB <- ReadAffy(filenames=array.files[batch=="B"], verbose=TRUE)
celp <- merge.AffyBatch(celA,celB)

grp<-list(grpA,grpB)
pooledgrp<-c(grpA,grpB)
       
setwd(globalwd)
source("cvfun.R")
set.seed(12345)
# be sure to have lot of memory - I use 20 gigs #
# this will take upwards of several hours #
cvfun(filename="Section4_GSE2034",nrep=100,nbatch=2,
	  celA=celA,
	  celB=celB,
	  grp=grp,
	  celp=celp,pooledgrp=pooledgrp
)
