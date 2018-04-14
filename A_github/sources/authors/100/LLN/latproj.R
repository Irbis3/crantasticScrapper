`latproj` <-
function(prms,Y,ind,method="SANN"){
# Find the ML positions of the new nodes
	Z <- prms$Z
	alpha <- prms$alpha
	p <- ncol(Z)
	nz <- nrow(Y)
	nx <- length(ind)
	flag <- c(matrix(1,1,nz-nx),matrix(2,1,nx))
	
	# find an initial position
	x <- matrix(0,nx,p);
	y <- matrix(0,nx,nz);
	y <- Y[ind,-ind]; y <- matrix(y,nx,nz-nx)
	for (i in 1:nx){
		for (j in 1:p){
			if (sum(y[i,]==1) > 0){
				x[i,j] <- mean(c(max(Z[y[i,]==1,j]),min(Z[y[i,]==1,j])))
			}
			else {
				x[i,j] <- mean(c(max(Z[,j]),min(Z[,j])))
			}
		}
	}

	# Save the initial position
	Zinit <- rbind(Z,matrix(x,nrow=nx,ncol=p))
	
	# Optimization step (method="SANN" or "BFGS")
	res <- optim(x,mloglik,alpha=alpha,Y=Y,Z=Z,method=method)	
	ZZ = rbind(Z,matrix(res$par,nrow=nx,ncol=p))
	
	# Return the result
	prms <- list(alpha=alpha,Z=ZZ,Y=Y,flag=flag,Zinit=Zinit,optim=res)
	class(prms) <- "lln"
	prms
}

