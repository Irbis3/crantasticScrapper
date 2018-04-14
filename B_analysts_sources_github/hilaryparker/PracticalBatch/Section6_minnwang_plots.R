## creates PDF with all of the figures for each experiment
minnwang_figs<-function(globalwd){
	setwd(globalwd)
	load("GSE2034_Section6_bgadj_model.RData")
	
	r2.2034<-r2
	coefs.2034<-coefs
	
	load("GSE2603_Section6_bgadj_model.RData")
	
	r2.2603<-r2
	coefs.2603<-coefs
	
	coefslist<-list(coefs.2034,coefs.2603)
	
	temp<-c(as.vector(coefs.2034),as.vector(coefs.2603))
	nms<-c("Wang et al. 2005","Minn et al. 2005")

	
	pdf(file="minnwang_coef_plots.pdf",width=14)
	par(mfrow=c(1,2))
	for(i in 1:2){
	
		########### Getting figure ##############
		effects <- matrix(0, nrow = 25, ncol = 4)
		colnames(effects) <- c("A", "C", "G", "T")
		effects[,1]<-coefslist[[i]][1:25]
		effects[,2]<-coefslist[[i]][26:50]
		effects[,3]<-coefslist[[i]][51:75]
		effects <- sweep(effects, 1, rowMeans(effects))
		
		pos<-rep(1:25,4)
		plot(x=pos,y=effects,pch=".",col="white",xlab="Position",ylab="Effect",
			ylim=c(-1,1),
			cex.axis=1.5,
			cex.lab=1.5,
			cex.main=1.5,
			main=nms[i]
			)
		points(x=1:25,y=effects[,1],pch="A",col=2)
		points(x=1:25,y=effects[,2],pch="C",col=3)
		points(x=1:25,y=effects[,3],pch="G",col=4)
		points(x=1:25,y=effects[,4],pch="T",col=5)
	}
	dev.off()
	
}