cvplot <- function(nrep,
				   nbatch){
	
	library(plotrix)
	library(fields)
	x<-c()
	within.full <- list(x,x,x,x,x,x)
	between.full <- list(x,x,x,x,x,x)

	in1<-0
	in2<-0

	for(i in 1:nrep){
		for(a in 1:nbatch){
			for(b in 1:nbatch){
				if(a==b){
					in1<-in1+1
					within.full[[1]][[in1]] <- rma.tsp.result[[i]][a,b]
					within.full[[2]][[in1]] <- frma.tsp.result[[i]][a,b]
					within.full[[3]][[in1]] <- rma.pam.result[[i]][a,b]
					within.full[[4]][[in1]] <- frma.pam.result[[i]][a,b]
					within.full[[5]][[in1]] <- rma.lasso.result[[i]][a,b]
					within.full[[6]][[in1]] <- frma.lasso.result[[i]][a,b]
				}
				else {
					in2<-in2+1
					between.full[[1]][[in2]] <- rma.tsp.result[[i]][a,b]
					between.full[[2]][[in2]] <- frma.tsp.result[[i]][a,b]
					between.full[[3]][[in2]] <- rma.pam.result[[i]][a,b]
					between.full[[4]][[in2]] <- frma.pam.result[[i]][a,b]
					between.full[[5]][[in2]] <- rma.lasso.result[[i]][a,b]
					between.full[[6]][[in2]] <- frma.lasso.result[[i]][a,b]
				}
			}	
		}	
	}
    
	a1<-within.full[[5]]
	a2<-between.full[[5]]
	a3<-pooled.rma.lasso.result
	a4<-within.full[[1]]
	a5<-between.full[[1]]
	a6<-pooled.rma.tsp.result
	a7<-within.full[[3]]
	a8<-between.full[[3]]
	a9<-pooled.rma.pam.result

	b1<-within.full[[6]]
	b2<-between.full[[6]]
	b3<-pooled.frma.lasso.result
	b4<-within.full[[2]]
	b5<-between.full[[2]]
	b6<-pooled.frma.tsp.result
	b7<-within.full[[4]]
	b8<-between.full[[4]]
	b9<-pooled.frma.pam.result	

	
	##get death rate baseline for comparison##
	hl <- sum(pooledgrp)/length(pooledgrp)
	hl <- max(hl,1-hl)
	
	######### Full results ###########
	y<-list(a1,a2,a3,a4,a5,a6,a7,a8,a9,NA,
			b1,b2,b3,b4,b5,b6,b7,b8,b9)

			
	library(RColorBrewer)
	mypar <- function(a=1,b=1,brewer.n=8,brewer.name="Dark2",...){
		par(mar=c(2.5,2.5,1.6,1.1),mgp=c(1.5,.5,0))
		par(mfrow=c(a,b),...)
		palette(brewer.pal(brewer.n,brewer.name))
	}
	mypar(1)


	col<-c(rep(1:3,3),"white")

	boxplot(y,
		las=1,
		col=col,
		varwidth=TRUE,
		outpch=20,
		outcex=0.5,
		lwd=2,
		medlwd=2,
		whisklty = "solid",
		xaxt='n',
		cex.axis=1.3)
		
		
	abline(v=10,lty=3,lwd=2)
	#abline(h=hl,lty=2)

	axis(side=3, at = c(5,15), labels=c("RMA","fRMA"), tick=FALSE, lwd=2, cex.axis=1.3)
	axis(side=1, at = c(-0.75,2,5,8,12,15,18,20.5), 
	     labels=c("",rep(c("LASSO","TSP","PAM"),2),""), lwd=2, cex.axis=1.3)


	legend("bottomright",c("Within","Between","Pooled"),col=c(1:3),
			pch=c(15,15,15),
			pt.cex=2,
			lty=c(0,0,0),
			lwd=c(0,0,0),
			cex=1.3)
			
	legend("bottomright",c("Within","Between","Pooled"),col="black",
			pch=c(22,22,22),
			pt.cex=2,
			lwd=c(2,2,2),
			lty=c(0,0,0),
			cex=1.3)
			
	legend("bottomleft",paste(nrep,"Iterations"),cex=1.3)
}