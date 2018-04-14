
confplot_reduced<-function(filename="Section6_GSE2034"){

	ind<-c(10,20,30,40,50,60)
	temp<-rep(0,length(ind))
	Q1<-list(temp,temp,temp,temp)
	Q2<-list(temp,temp,temp,temp)
	Q3<-list(temp,temp,temp,temp)

	for(z in 1:length(ind)){
	
	
	#fRMA-PAM#
		load(paste(filename,"_fRMA_PAM_",ind[z],".RData",sep=""))
		len<-length(test.pred.group)
		for(i in 1:len){
			test.pred.group[[i]]<-as.numeric(test.pred.group[[i]])-1
		}
		res<-c()
		for(j in 1:len){
			res[j]<-sum(test.pred.group[[j]]==testgrp[[j]])/length(testgrp[[j]])
		}
		res<-summary(res)
		Q1[[1]][z]<-res[2]
		Q2[[1]][z]<-res[3]
		Q3[[1]][z]<-res[5]
		
		
	#RMA-PAM#
		load(paste(filename,"_RMA_PAM_",ind[z],".RData",sep=""))
		for(i in 1:len){
			test.pred.group[[i]]<-as.numeric(test.pred.group[[i]])-1
		}
		res<-c()
		for(j in 1:len){
			res[j]<-sum(test.pred.group[[j]]==testgrp[[j]])/length(testgrp[[j]])
		}
		res<-summary(res)
		Q1[[2]][z]<-res[2]
		Q2[[2]][z]<-res[3]
		Q3[[2]][z]<-res[5]
		
	#fRMA-TSP#
		load(paste(filename,"_fRMA_TSP_",ind[z],".RData",sep=""))
		res<-c()
		for(j in 1:len){
			res[j]<-sum(test.pred.group[[j]]==testgrp[[j]])/length(testgrp[[j]])
		}
		res<-summary(res)
		Q1[[3]][z]<-res[2]
		Q2[[3]][z]<-res[3]
		Q3[[3]][z]<-res[5]
		
		
	#RMA-TSP#	
		load(paste(filename,"_RMA_TSP_",ind[z],".RData",sep=""))
		res<-c()
		for(j in 1:len){
			res[j]<-sum(test.pred.group[[j]]==testgrp[[j]])/length(testgrp[[j]])
		}
		res<-summary(res)
		Q1[[4]][z]<-res[2]
		Q2[[4]][z]<-res[3]
		Q3[[4]][z]<-res[5]
	}
		
	ref<-c(Q1[[1]],Q2[[1]],Q3[[1]],
		   Q1[[2]],Q2[[2]],Q3[[2]],
		   Q1[[3]],Q2[[3]],Q3[[3]],
		   Q1[[4]],Q2[[4]],Q3[[4]]
		   )
			
	pdf(file=paste(filename,"_confplot_reduced.pdf",sep=""),width=14,height=14)
	par(mfrow=c(2,2))
	
	#fRMA-PAM#
	plot(x=ind,y=Q2[[1]],ylim=c(0.65,1.00),type="l",cex.lab=1.5,cex.main=1.5,cex.axis=1.5,lwd=2,
			 xlab="Percentage of Probes Removed",ylab="Prediction Accuracy",main="fRMA-PAM")
	lines(x=ind,y=Q1[[1]],lty=2,lwd=2,col="darkgrey")
	lines(x=ind,y=Q3[[1]],lty=2,lwd=2,col="darkgrey")
	legend("bottomright",legend=c("75 Percentile","Median","25 Percentile"),lty=c(2,1,2),
		   col=c("darkgrey","black","darkgrey"),lwd=2,cex=1.5)

	#RMA-PAM#
	plot(x=ind,y=Q2[[2]],ylim=c(0.65,1.00),type="l",cex.lab=1.5,cex.main=1.5,cex.axis=1.5,lwd=2,
			 xlab="Percentage of Probes Removed",ylab="Prediction Accuracy",main="RMA-PAM")
	lines(x=ind,y=Q1[[2]],lty=2,lwd=2,col="darkgrey")
	lines(x=ind,y=Q3[[2]],lty=2,lwd=2,col="darkgrey")
	legend("bottomright",legend=c("75 Percentile","Median","25 Percentile"),lty=c(2,1,2),
		   col=c("darkgrey","black","darkgrey"),lwd=2,cex=1.5)

	#fRMA-TSP#
	plot(x=ind,y=Q2[[3]],ylim=c(0.65,1.00),type="l",cex.lab=1.5,cex.main=1.5,cex.axis=1.5,lwd=2,
			 xlab="Percentage of Probes Removed",ylab="Prediction Accuracy",main="fRMA-TSP")
	lines(x=ind,y=Q1[[3]],lty=2,lwd=2,col="darkgrey")
	lines(x=ind,y=Q3[[3]],lty=2,lwd=2,col="darkgrey")
	legend("bottomright",legend=c("75 Percentile","Median","25 Percentile"),lty=c(2,1,2),
		   col=c("darkgrey","black","darkgrey"),lwd=2,cex=1.5)
	
	
	#RMA-TSP#	
	plot(x=ind,y=Q2[[4]],ylim=c(0.65,1.00),type="l",cex.lab=1.5,cex.main=1.5,cex.axis=1.5,lwd=2,
			 xlab="Percentage of Probes Removed",ylab="Prediction Accuracy",main="RMA-TSP")
	lines(x=ind,y=Q1[[4]],lty=2,lwd=2,col="darkgrey")
	lines(x=ind,y=Q3[[4]],lty=2,lwd=2,col="darkgrey")
	legend("bottomright",legend=c("75 Percentile","Median","25 Percentile"),lty=c(2,1,2),
		   col=c("darkgrey","black","darkgrey"),lwd=2,cex=1.5)

	dev.off()
}
