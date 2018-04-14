get_densities<-function(filename){
	
	load(paste("Section6_",filename,"_model_coefs.RData",sep=""))
	
	rma_batch_coefs_tot<-c(0)
	frma_batch_coefs_tot<-c(0)
	rma_out_coefs_tot<-c(0)
	frma_out_coefs_tot<-c(0)
	for(i in 1:100){
		rma_batch_coefs_tot<-cbind(rma_batch_coefs_tot, rma_batch_coefs[[i]])
		frma_batch_coefs_tot<-cbind(frma_batch_coefs_tot, frma_batch_coefs[[i]])
		rma_out_coefs_tot<-cbind(rma_out_coefs_tot, rma_out_coefs[[i]])
		frma_out_coefs_tot<-cbind(frma_out_coefs_tot, frma_out_coefs[[i]])
	}
	rma_batch_coefs_tot<-rma_batch_coefs_tot[-1]
	frma_batch_coefs_tot<-rma_batch_coefs_tot[-1]
	rma_out_coefs_tot<-rma_out_coefs_tot[-1]
	frma_out_coefs_tot<-rma_out_coefs_tot[-1]
	
	pdf(file=paste(filename,"_total_densities.pdf",sep=""),width=14)
	par(mfrow=c(1,2))
	plot(density(rma_out_coefs_tot),xlim=c(-15,15),main="RMA",xlab="t Value",lwd=2,
		cex.lab=1.3,cex.axis=1.3,cex.main=1.5)
	lines(density(rma_batch_coefs_tot),col=2,lwd=2)
	legend("topright",legend=c("Outcome Coefs","Batch Coefs"),lty=1,col=1:2,lwd=2,cex=1.3)
	
	plot(density(frma_out_coefs_tot),xlim=c(-15,15),main="fRMA",xlab="t Value",lwd=2,
		cex.lab=1.3,cex.axis=1.3,cex.main=1.5)
	lines(density(frma_batch_coefs_tot),col=2,lwd=2)
	legend("topright",legend=c("Outcome Coefs","Batch Coefs"),lty=1,col=1:2,lwd=2,cex=1.3)
	dev.off()
}