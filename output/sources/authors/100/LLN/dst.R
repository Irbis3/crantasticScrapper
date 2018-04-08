`dst` <-
function(Y){
	#calculates distances between nodes based on path length
	#returns d=g if nodes are not connected
	g <- dim(Y)[1]
	Dst<-Yr<-Y
	Dst<-Y*(Y==1) + g*(Y==0)
	for(r in 2:(g-1)) {
		Yr<-Yr%*%Y
		Dst<-Dst+(r-g)*( Yr>0 & Dst==g )
	}
	diag(Dst)<-0
	Dst
}

