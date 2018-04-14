`lpz.dist` <-
function(Z){
	#gives the negative distance between nodes
	ZtZ<-Z%*%t(Z)
	
	mg<-as.matrix(diag(ZtZ))%*%rep(1,length(Z[,1])) #distances
	mg<-mg+t(mg)
	d<-sqrt((mg-2*ZtZ))
	-d
}

