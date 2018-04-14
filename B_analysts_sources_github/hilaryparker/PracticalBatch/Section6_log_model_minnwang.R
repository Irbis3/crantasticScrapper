
globalwd<-"/home/bst/student/hiparker/PracticalBatch"
setwd(globalwd)
## working directory of your files ##
#Packages I'll need#
library(affy)
library(gcrma)
#library(oligo)
library(genefilter)
library(preprocessCore)
	
#load housekeeping data that will be used to read in arrays#
load("Section3_GSE2034.RData")
grp<-grp[batch=="A"|batch=="B"]
array.files<-array.files[batch=="A"|batch=="B"]
batch<-batch[batch=="A"|batch=="B"]
fileloc<-"/home/bst/student/hiparker/ErrorRateProject/GSE2034/celfiles"
setwd(fileloc)
pvals<-c()

celAB <- ReadAffy(filenames=array.files, verbose=TRUE)
x2<-exprs(bg.adjust.gcrma(celAB))  #this takes some time#
len<-dim(x2)[1]
pvals<-rowttests(x=log2(x2),fac=as.factor(batch))$p.value
	
			
############## GET PROBE SEQUENCE DATA ################
source("http://bioconductor.org/biocLite.R")
biocLite("hgu133aprobe")
library("hgu133aprobe")
data(hgu133aprobe)
probes<-as.data.frame(hgu133aprobe)
########
		
probes$Index <- xy2indices(x=probes$x,y=probes$y,abatch=celAB)
		
log10pvals<- -log10(pvals[probes$Index])
		
library(oligo)
X <- sequenceDesignMatrix(probes$sequence)
fit <- lm(log10pvals~X)
coefs <- fit$coef[-1]
ssreg <- sum((fit$fitted-mean(log10pvals))^2)
sstot <- sum((log10pvals-mean(log10pvals))^2)
r2 <- ssreg/sstot
		
setwd(globalwd)
save("coefs","r2",
	file="GSE2034_Section6_bgadj_model.RData")

rm(list=ls())







#########################	
globalwd<-"/home/bst/student/hiparker/PracticalBatch"
setwd(globalwd)
## working directory of your files ##
#Packages I'll need#
library(affy)
library(gcrma)
#library(oligo)
library(genefilter)
library(preprocessCore)
	
#load housekeeping data that will be used to read in arrays#
load("Section3_GSE2603.RData")
grp<-grp[batch=="A"|batch=="B"]
array.files<-array.files[batch=="A"|batch=="B"]
batch<-batch[batch=="A"|batch=="B"]
fileloc<-"/nexsan/bst2/microarray/CEL/GPL96/"
setwd(fileloc)
pvals<-c()

celAB <- ReadAffy(filenames=array.files[batch=="A"|batch=="B"], verbose=TRUE)
x2<-exprs(bg.adjust.gcrma(celAB))  #this takes some time#
len<-dim(x2)[1]

pvals<-rowttests(x=log2(x2),fac=as.factor(batch))$p.value

	
			
############## GET PROBE SEQUENCE DATA ################
source("http://bioconductor.org/biocLite.R")
biocLite("hgu133aprobe")
library("hgu133aprobe")
data(hgu133aprobe)
probes<-as.data.frame(hgu133aprobe)
########
		
probes$Index <- xy2indices(x=probes$x,y=probes$y,abatch=celAB)
		
log10pvals<- -log10(pvals[probes$Index])
		
library(oligo)
X <- sequenceDesignMatrix(probes$sequence)
fit <- lm(log10pvals~X)
coefs <- fit$coef[-1]
ssreg <- sum((fit$fitted-mean(log10pvals))^2)
sstot <- sum((log10pvals-mean(log10pvals))^2)
r2 <- ssreg/sstot
		
setwd(globalwd)
save("coefs","r2",
	file="GSE2603_Section6_bgadj_model.RData")