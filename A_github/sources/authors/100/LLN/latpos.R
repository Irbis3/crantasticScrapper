`latpos` <-
function(Y,p=2,alpha=0,method="SANN"){
# Build a latent representation of the network
	n <- dim(Y)[1]    #number of nodes

	# find unconnected nodes
	zro <- c()
	for (i in 1:n){ if (sum(Y[i,]) < 1) zro <- c(zro,i)}
	if (length(zro) > 0) warning('There is at least one unconnected node in the network!')

	# starting values of Z and alpha
	D <- dst(Y)
	Z <- cmdscale(D,p)
	for (i in 1:p){
		Z[,i] <- Z[,i] - mean(Z[,i])
	}

	# now find a good starting point for the MCMC
	avZ <- c(alpha,c(Z))

	# Optimization step (method="SANN" or "BFGS")
	res <- optim(avZ,mlpY,Y=Y,method=method)
	avZ <- res$par

	# BIC value
	bic <- -2*res$value - (n*p+1)*log(sum(Y)/2)

	# Return final parameters
	prms  <- list(alpha=avZ[1],Z=matrix(avZ[-1],nrow=n,ncol=p),Y=Y,flag=c(matrix(1,1,n)),bic=bic,optim=res)
	class(prms) <- "lln"
	prms
}

