`mlpY` <-
function(avZ,Y,p){
	#minus log prob of graph, with Z in vector form, 
	#to be used in by optim or nlm
	n <- dim(Y)[1]    #number of nodes
	alpha <- avZ[1]
	Z <- matrix(avZ[-1],nrow=n,ncol=p)
	lpZ <- lpz.dist(Z)    #the function of Z that is in the linear predictor
	lpg <- alpha+lpZ
	diag(lpg) <- 0
	-(  sum( Y*lpg - log( 1+exp(lpg) ) ) + dim(Y)[1]*log(2)  )
}

