cvtable <- function(nrep,
				   nbatch){

	in1<-0
	in2<-0
	x<-c()
	within.full <- list(x,x,x,x,x,x)
	between.full <- list(x,x,x,x,x,x)

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
    
	
	rma.tsp.within<-within.full[[1]]
	rma.tsp.between<-between.full[[1]]
	rma.tsp.pooled<-pooled.rma.tsp.result
	rma.pam.within<-within.full[[3]]
	rma.pam.between<-between.full[[3]]
	rma.pam.pooled<-pooled.rma.pam.result
	rma.lasso.within<-within.full[[5]]
	rma.lasso.between<-between.full[[5]]
	rma.lasso.pooled<-pooled.rma.lasso.result

	frma.tsp.within<-within.full[[2]]
	frma.tsp.between<-between.full[[2]]
	frma.tsp.pooled<-pooled.frma.tsp.result
	frma.pam.within<-within.full[[4]]
	frma.pam.between<-between.full[[4]]
	frma.pam.pooled<-pooled.frma.pam.result	
	frma.lasso.within<-within.full[[6]]
	frma.lasso.between<-between.full[[6]]
	frma.lasso.pooled<-pooled.frma.lasso.result
	
	
	
	res<-list(
		rma.lasso.within,
		rma.lasso.between,
		rma.lasso.pooled,
		
		frma.lasso.within,
		frma.lasso.between,
		frma.lasso.pooled,
	
		rma.tsp.within,
		rma.tsp.between,
		rma.tsp.pooled,
		
		frma.tsp.within,
		frma.tsp.between,
		frma.tsp.pooled,
		
		rma.pam.within,
		rma.pam.between,
		rma.pam.pooled,
		
		frma.pam.within,
		frma.pam.between,
		frma.pam.pooled
	)
	
	len <- length(res)
	
	tab<-c()
	for(i in 1:len){
		summ<-signif(summary(res[[i]]),3)
		tab[i]<-paste(summ[3]," (",summ[2],", ",summ[5],")",sep="")
	}
	
	tab<-matrix(tab,byrow=TRUE,nrow=6,ncol=3)
	tab<-data.frame(tab)
	rownames(tab)<-c("RMA-Lasso","fRMA-Lasso","RMA-TSP","fRMA-TSP","RMA-PAM","fRMA-PAM")
	colnames(tab)<-c("Within","Between","Pooled")
	print(tab)
	
}