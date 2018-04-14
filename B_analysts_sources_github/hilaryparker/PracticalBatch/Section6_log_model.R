log_model<-function(globalwd,strt,fin,bgadj=FALSE){
	setwd(globalwd)
## working directory of your files ##
	#Packages I'll need#
	library(affy)
	library(gcrma)
	#library(oligo)
	library(genefilter)
	library(preprocessCore)
	
	#load housekeeping data that will be used to read in arrays#
	load("Section6_multi_prelim.RData")
	#arr.names - list of array names by experiment
	#batches - list of batch indicators by experiment
	#dates - list of julian dates by experiment (won't actually need)

	coefs<-list()
	r2<-c()
	len<-length(arr.names)
	
	
	for(i in strt:fin){
		print(i)
		setwd("/nexsan/bst2/microarray/CEL/GPL570/")
		afbatch <- ReadAffy(filenames=arr.names[[i]], verbose=TRUE)
		if(bgadj==TRUE){
			x2<-exprs(bg.adjust.gcrma(afbatch))  #this takes some time#
		}
		setwd(globalwd)
		batch<-batches[[i]]
		celdates<-dates[[i]]
		arr.name<-arr.names[[i]]

#		avg.intensity<-rowMeans(x)
		if(bgadj==FALSE){
			pvals<-rowttests(x=log2(normalize.quantiles(exprs(afbatch))),
							fac=as.factor(batches[[i]]))$p.value
		}
		if(bgadj==TRUE){
			pvals<-rowttests(x=log2(x2),
							fac=as.factor(batches[[i]]))$p.value
		}
				
		############## GET PROBE SEQUENCE DATA ################
		library("hgu133plus2probe")
		data(hgu133plus2probe)
		probes<-as.data.frame(hgu133plus2probe)
		########
		
		probes$Index <- xy2indices(x=probes$x,y=probes$y,abatch=afbatch)
		
		log10pvals<- -log10(pvals[probes$Index])
		
		library(oligo)
		X <- sequenceDesignMatrix(probes$sequence)
		fit <- lm(log10pvals~X)
		coefs <- fit$coef[-1]
		ssreg <- sum((fit$fitted-mean(log10pvals))^2)
		sstot <- sum((log10pvals-mean(log10pvals))^2)
		r2 <- ssreg/sstot
		
		setwd(paste(globalwd,"/model_res",sep=""))
		if(bgadj==TRUE){
		save("coefs","r2",
			file=paste(exper.index[i],"_Section6_bgadj.RData",sep=""))
		}
		if(bgadj==FALSE){
		save("coefs","r2",	
			file=paste(exper.index[i],"_Section6_nobgadj.RData",sep=""))
		}
	}
}


