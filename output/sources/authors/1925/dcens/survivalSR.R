survivalSR <-
function(procData,ST){

	n <- procData$n
	O <- procData$times[,1]
	L <- procData$times[,3]

	r <- length(O)

	S <- ST[,2]  ## Length = r+1 (S[0] included)

	## Jumps construction
	g <- ifelse(L==0,0.0,L/(n*S[2:(r+1)]))

	## Survival construction
	G <- rep(0,r)

	G[1] <- 1

	for(k in 2:r)
		G[k] <- G[k-1] - g[k-1]


	SR <- array(0,c(r+1,2))
	
	SR[1,1] <- 0
	SR[2:(r+1),1] <- O
	SR[1:r,2] <- G
	SR[r+1,2] <- G[r]
	if (procData$rightCensoring){
		SR[r+1,2] <- 0
	}

	return(SR)

}

