survivalSL <-
function(procData,ST){

	n <- procData$n
	O <- procData$times[,1]
	M <- procData$times[,2]

	r <- length(O)

	S <- ST[,2]  ## Length = r+1 (S[0] included)

	## Jumps construction
	h <- ifelse(M==0,0.0,M/(n*(1-S[1:r])))

	## Survival construction
	H <- rep(0,r+1)

	H[r+1] <- 0
	
	for(k in (r):1)
		H[k] <- H[k+1] + h[k]

	SL <- array(0,c(r+1,2))

	SL[1,1] <- 0
	SL[2:(r+1),1] <- O
	SL[,2]<-H
	if (procData$leftCensoring){
		SL[1,2] <- 1
	}

	return(SL)
}

