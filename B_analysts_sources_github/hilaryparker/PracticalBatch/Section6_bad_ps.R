## Get bad probesets for Section 6 trimming, plus plots


bad_ps_fun<-function(
			  celA,celB,
			  frmaA,frmaB,
			  direct,
			  filename){

	setwd(direct)
	library(affy)

	### REMOVE BATCHY PROVES!! ###


	rma.bad.ps.1<-list()
	rma.bad.ps.2<-list()
	rma.bad.ps.3<-list()
	rma.bad.ps.4<-list()
	rma.bad.ps.5<-list()
	rma.bad.ps.6<-list()

	frma.bad.ps.1<-list()
	frma.bad.ps.2<-list()
	frma.bad.ps.3<-list()
	frma.bad.ps.4<-list()
	frma.bad.ps.5<-list()
	frma.bad.ps.6<-list()

	rma_batch_coefs<-list(0,0,0,0,0,0,0,0,0,0,
						  0,0,0,0,0,0,0,0,0,0,
						  0,0,0,0,0,0,0,0,0,0,
						  0,0,0,0,0,0,0,0,0,0,
						  0,0,0,0,0,0,0,0,0,0,
						  0,0,0,0,0,0,0,0,0,0,
						  0,0,0,0,0,0,0,0,0,0,
						  0,0,0,0,0,0,0,0,0,0,
						  0,0,0,0,0,0,0,0,0,0,
						  0,0,0,0,0,0,0,0,0,0)
	rma_out_coefs<-list(0,0,0,0,0,0,0,0,0,0,
						  0,0,0,0,0,0,0,0,0,0,
						  0,0,0,0,0,0,0,0,0,0,
						  0,0,0,0,0,0,0,0,0,0,
						  0,0,0,0,0,0,0,0,0,0,
						  0,0,0,0,0,0,0,0,0,0,
						  0,0,0,0,0,0,0,0,0,0,
						  0,0,0,0,0,0,0,0,0,0,
						  0,0,0,0,0,0,0,0,0,0,
						  0,0,0,0,0,0,0,0,0,0)
	frma_batch_coefs<-list(0,0,0,0,0,0,0,0,0,0,
						  0,0,0,0,0,0,0,0,0,0,
						  0,0,0,0,0,0,0,0,0,0,
						  0,0,0,0,0,0,0,0,0,0,
						  0,0,0,0,0,0,0,0,0,0,
						  0,0,0,0,0,0,0,0,0,0,
						  0,0,0,0,0,0,0,0,0,0,
						  0,0,0,0,0,0,0,0,0,0,
						  0,0,0,0,0,0,0,0,0,0,
						  0,0,0,0,0,0,0,0,0,0)
	frma_out_coefs<-list(0,0,0,0,0,0,0,0,0,0,
						  0,0,0,0,0,0,0,0,0,0,
						  0,0,0,0,0,0,0,0,0,0,
						  0,0,0,0,0,0,0,0,0,0,
						  0,0,0,0,0,0,0,0,0,0,
						  0,0,0,0,0,0,0,0,0,0,
						  0,0,0,0,0,0,0,0,0,0,
						  0,0,0,0,0,0,0,0,0,0,
						  0,0,0,0,0,0,0,0,0,0,
						  0,0,0,0,0,0,0,0,0,0)

	for(i in 1:length(sepA)){
		## 1 and 3 are the build sets ##
		celAb<-celA[,sepA[[i]]==1 | sepA[[i]]==3]
		celBb<-celB[,sepB[[i]]==1 | sepB[[i]]==3]

		lenA<-length(celAb)
		lenB<-length(celBb)
		batch<-c(rep("A",lenA),rep("B",lenB))

		rma_datAB<-merge.AffyBatch(celAb,celBb)
		rma_datAB<-exprs(rma(rma_datAB))

		frma_datAB<-cbind(frmaA[,sepA[[i]]==1 | sepA[[i]]==3],
						  frmaB[,sepB[[i]]==1 | sepB[[i]]==3])

		modgrp<-c(grp[[1]][sepA[[i]]==1 | sepA[[i]]==3],
				  grp[[2]][sepB[[i]]==1 | sepB[[i]]==3])

		rma_pvals<-c()

		quants<-seq(.1,.6,by=.1)

		len<-dim(frma_datAB)[1]


		#RMA first#
		for(s in 1:len){
			out<-as.vector(rma_datAB[s,])
			fit<-lm(out~as.factor(batch)+as.factor(modgrp))
			rma_pvals[s]<-coef(summary(fit))[2,4]
			rma_batch_coefs[[i]][s]<-coef(summary(fit))[2,3]
			rma_out_coefs[[i]][s]<-coef(summary(fit))[3,3]
		}

		rma_threshs<-quantile(rma_pvals, quants)

		frma_pvals<-c()

		#fRMA next#
		for(s in 1:len){
			out<-as.vector(frma_datAB[s,])
			fit<-lm(out~as.factor(batch)+as.factor(modgrp))
			frma_pvals[s]<-coef(summary(fit))[2,4]
			frma_batch_coefs[[i]][s]<-coef(summary(fit))[2,3]
			frma_out_coefs[[i]][s]<-coef(summary(fit))[3,3]
		}

		frma_threshs<-quantile(frma_pvals, quants)

		## RMA bad probesets ##
		bad.ps.ind.1<-rep(0,len)
		bad.ps.ind.2<-rep(0,len)
		bad.ps.ind.3<-rep(0,len)
		bad.ps.ind.4<-rep(0,len)
		bad.ps.ind.5<-rep(0,len)
		bad.ps.ind.6<-rep(0,len)

		bad.ps.ind.1[which(rma_pvals<rma_threshs[1])]<-1
		bad.ps.ind.2[which(rma_pvals<rma_threshs[2])]<-1
		bad.ps.ind.3[which(rma_pvals<rma_threshs[3])]<-1
		bad.ps.ind.4[which(rma_pvals<rma_threshs[4])]<-1
		bad.ps.ind.5[which(rma_pvals<rma_threshs[5])]<-1
		bad.ps.ind.6[which(rma_pvals<rma_threshs[6])]<-1

		rma.bad.ps.1[[i]]<-bad.ps.ind.1
		rma.bad.ps.2[[i]]<-bad.ps.ind.2
		rma.bad.ps.3[[i]]<-bad.ps.ind.3
		rma.bad.ps.4[[i]]<-bad.ps.ind.4
		rma.bad.ps.5[[i]]<-bad.ps.ind.5
		rma.bad.ps.6[[i]]<-bad.ps.ind.6


		## fRMA bad probesets ##
		bad.ps.ind.1<-rep(0,len)
		bad.ps.ind.2<-rep(0,len)
		bad.ps.ind.3<-rep(0,len)
		bad.ps.ind.4<-rep(0,len)
		bad.ps.ind.5<-rep(0,len)
		bad.ps.ind.6<-rep(0,len)

		bad.ps.ind.1[which(frma_pvals<frma_threshs[1])]<-1
		bad.ps.ind.2[which(frma_pvals<frma_threshs[2])]<-1
		bad.ps.ind.3[which(frma_pvals<frma_threshs[3])]<-1
		bad.ps.ind.4[which(frma_pvals<frma_threshs[4])]<-1
		bad.ps.ind.5[which(frma_pvals<frma_threshs[5])]<-1
		bad.ps.ind.6[which(frma_pvals<frma_threshs[6])]<-1

		frma.bad.ps.1[[i]]<-bad.ps.ind.1
		frma.bad.ps.2[[i]]<-bad.ps.ind.2
		frma.bad.ps.3[[i]]<-bad.ps.ind.3
		frma.bad.ps.4[[i]]<-bad.ps.ind.4
		frma.bad.ps.5[[i]]<-bad.ps.ind.5
		frma.bad.ps.6[[i]]<-bad.ps.ind.6
	}

	save(list=c('rma.bad.ps.1',
				'rma.bad.ps.2',
				'rma.bad.ps.3',
				'rma.bad.ps.4',
				'rma.bad.ps.5',
				'rma.bad.ps.6'),
		file=paste(filename,"_RMA_badps.RData",sep=""))

	save(list=c('frma.bad.ps.1',
				'frma.bad.ps.2',
				'frma.bad.ps.3',
				'frma.bad.ps.4',
				'frma.bad.ps.5',
				'frma.bad.ps.6'),
		file=paste(filename,"_fRMA_badps.RData",sep=""))

	save(list=c("rma_batch_coefs","frma_batch_coefs","rma_out_coefs","frma_out_coefs"),
		 file=paste(filename,"_model_coefs.RData",sep=""))
}