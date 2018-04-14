## creates PDF with all of the figures for each experiment
# plus histogram of R2 values
multi_figs<-function(globalwd,bgadj=TRUE){
	setwd(globalwd)
	load("Section6_multi_prelim.RData")
	setwd(paste(globalwd,"/model_res",sep=""))
	files<-dir()
	ind<-grep("nobg",files)
	if(bgadj==FALSE){
		files<-files[ind]
	}
	if(bgadj==TRUE){
		files<-files[-ind]
	}
	
	len<-length(files)
	temp<-0
	r2s<-c()
	coefslist<-list()
	for(i in 1:len){
		load(files[i])
		temp<-c(temp,as.vector(coefs))
		coefslist[[i]]<-coefs
		r2s[i]<-r2
	}
	ymin<-min(temp)
	ymax<-max(temp)
	
	
	
	setwd(globalwd)
	if(bgadj==TRUE){
		pdf(file=("fitgraphs_multi_bgadj.pdf"))
	}
	if(bgadj==FALSE){
		pdf(file=("fitgraphs_multi_nobgadj.pdf"))
	}
	
	for(i in 1:len){
	
		########### Getting figure ##############
		effects <- matrix(0, nrow = 25, ncol = 4)
		colnames(effects) <- c("A", "C", "G", "T")
		effects[,1]<-coefslist[[i]][1:25]
		effects[,2]<-coefslist[[i]][26:50]
		effects[,3]<-coefslist[[i]][51:75]
		effects <- sweep(effects, 1, rowMeans(effects))
		
		pos<-rep(1:25,4)
		plot(x=pos,y=effects,pch=".",col="white",xlab="Position",ylab="Effect",
			ylim=c(ymin,ymax),
			cex.axis=1.5,
			cex.lab=1.5,
			cex.main=1.5,
			main=strsplit(files[i],"_")[[1]][1]
			)
		points(x=1:25,y=effects[,1],pch="A",col=2)
		points(x=1:25,y=effects[,2],pch="C",col=3)
		points(x=1:25,y=effects[,3],pch="G",col=4)
		points(x=1:25,y=effects[,4],pch="T",col=5)
	}
	dev.off()
	
	setwd(globalwd)
	load("GSE2034_Section6_bgadj_model.RData")
	
	r2.2034<-r2
	coefs.2034<-coefs
	
	load("GSE2603_Section6_bgadj_model.RData")
	
	r2.2603<-r2
	coefs.2603<-coefs
	
	if(bgadj==TRUE){
		pdf(file="bg_hist_r2s.pdf")
		hist(r2s, breaks=50, xlab="R2 Values", main=" ", col="gray",xlim=c(0,0.3),
		cex.lab=1.5,cex.axis=1.5)	
		abline(v=r2.2034,col="DeepPink",lwd=3)
		abline(v=r2.2603,col="blue",lwd=3)
		legend("bottomright",c("Wang et al.","Minn et al."),lwd=2,col=c("DeepPink","blue"))
		dev.off()
	}
	
	if(bgadj==FALSE){
		pdf(file="nobg_hist_r2s.pdf")
		hist(r2s, breaks=50, xlab="R2 Values", main=" ", col="gray",xlim=c(0,0.3),
		cex.lab=1.5,cex.axis=1.5)
		dev.off()
	}
		
}

## creates PDF with all of the figures for each experiment
# plus histogram of R2 values
multi_figs_examples<-function(globalwd,bgadj=TRUE){
	setwd(globalwd)
	load("Section6_multi_prelim.RData")
	setwd(paste(globalwd,"/model_res",sep=""))
	files<-dir()
	ind<-grep("nobg",files)
	if(bgadj==FALSE){
		files<-files[ind]
	}
	if(bgadj==TRUE){
		files<-files[-ind]
	}
	
	temp<-0
	r2s<-c()
	coefslist<-list()
	for(i in c(5,100,214)){
		load(files[i])
		temp<-c(temp,as.vector(coefs))
		coefslist[[i]]<-coefs
		r2s[i]<-r2
	}
	ymin<-min(temp)
	ymax<-max(temp)
	
	
	setwd(globalwd)
	if(bgadj==TRUE){
		pdf(file=("fitgraphs_multi_bgadj_examples.pdf"),height=3.5,width=8)
		par(mfrow=c(1,3))
	}
	if(bgadj==FALSE){
		pdf(file=("fitgraphs_multi_nobgadj_examples.pdf"),height=3.5,width=8)
		par(mfrow=c(1,3))
	}
	
	for(i in c(5,100,214)){
	
		########### Getting figure ##############
		effects <- matrix(0, nrow = 25, ncol = 4)
		colnames(effects) <- c("A", "C", "G", "T")
		effects[,1]<-coefslist[[i]][1:25]
		effects[,2]<-coefslist[[i]][26:50]
		effects[,3]<-coefslist[[i]][51:75]
		effects <- sweep(effects, 1, rowMeans(effects))
		
		pos<-rep(1:25,4)
		plot(x=pos,y=effects,pch=".",col="white",xlab="Position",ylab="Effect",
			ylim=c(ymin,ymax),
			cex.axis=1.5,
			cex.lab=1.5,
			cex.main=1.5,
			main=strsplit(files[i],"_")[[1]][1]
			)
		points(x=1:25,y=effects[,1],pch="A",col=2)
		points(x=1:25,y=effects[,2],pch="C",col=3)
		points(x=1:25,y=effects[,3],pch="G",col=4)
		points(x=1:25,y=effects[,4],pch="T",col=5)
	}
	dev.off()
	
		
}