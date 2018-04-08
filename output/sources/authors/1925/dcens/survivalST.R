survivalST <-
function(procData){

	## Times loading
	O <- procData$timesST[,1]
	M <- procData$timesST[,2]
	L <- procData$timesST[,3]
	D <- procData$timesST[,4]

	r <- length(O)
	n <- procData$n

	ind <- which(D!=0)
	t <- length(ind)
	
	## Calculation of the number of lambdas and mus
	sumL <- rep(0,t)
	sumM <- rep(0,t)

	u <- ind[t]
	sumL[t] <- L[u]

	for (i in (t-1):1){
		lloc0 <- ind[i]
		lloc1 <- ind[i+1]
		sumL[i] <- sum(L[lloc0:(lloc1-1)])
		sumM[i] <- sum(M[(lloc0+1):lloc1])
	}

	## Maximum of iterations
	MAX.ITERATIONS <- 100

	## Array to save the f0[t] of every iteration
	control<-array(0,c(2,MAX.ITERATIONS+1)) 


	## Calculation of f0

	f0 <- rep(0,t)
	F0 <- rep(0,t)

	f0[t]<-1.0/n
	F0[t]<-f0[t]
	ea<-f0[t]

	for (i in (t-1):1){
		lloc0 <- ind[i]
		lloc1 <- ind[i+1]
		mort0 <- D[lloc0]
		mort1 <- D[lloc1]
		sl <- sumL[i]
		sm <- sumM[i]
		aux <- sl/F0[i+1]- sm/(1-F0[i+1]) + mort1/f0[i+1]
		f0[i] <- mort0/aux
		F0[i] <- F0[i+1]+f0[i]
	}

	d <- 1.0-F0[1]


	## Loop to find convergence of f0

	ma<-10
 	ma2<-0

	j<-0	
	a<-0
	b<-0

	while(ma>0.0003 && j<MAX.ITERATIONS)
	{	
		j<-j+1
		control[1,j]<-f0[t]

 		if (ma2>0 | d<0){ 
			ma2<-0
			control[2,j]<-1
		}

		f1 <- rep(0,t)
		F1 <- rep(0,t)

		# Increasing 0.1 of f0[t] until f0[1] > 1		                   	
            if(max(control[2,])==0){ 
			f1[t] <- f0[t]+0.1
		}
		# If f0[1] > 1 for any iteration, we calculate the medium point between 
		# the 2 closer iterations where f0[1] > 1 and f0[1] < 1 respectively
		else{
			a<-max(control[1,control[2,]==0])
			b<-min(control[1,control[2,]==1])
			f1[t]=(a+b)/2
		}
					    
		F1[t] <- f1[t]
	
		for (i in (t-1):1){
			lloc0 <- ind[i]
			lloc1 <- ind[i+1]
			mort0 <- D[lloc0]
			mort1 <- D[lloc1]
			sl <- sumL[i]
			sm <- sumM[i]
			aux <- sl/F1[i+1] - sm/(1-F1[i+1]) + mort1/f1[i+1]
			f1[i] <- mort0/aux
			
			if (is.nan(f1[i]) | f1[i]<0){
				f1[i] <- 0
				ma2 <- 0.1
				break
			}

			F1[i] <- F1[i+1]+f1[i]
		}

		d <- 1.0-F1[1]
		ma <- max(abs(f0-f1),ma2,abs(d))
		f0 <- f1
		F0 <- F1	
    			
	}
	
	if(j==MAX.ITERATIONS){
		print("ERROR: Convergence not possible.")
		return(NULL)
	}

	## Jumps standardizing     
      F0<-F0/F1[1]
    
	## Construction of the ST survival assuming exact data at the extremes
	S <- rep(0,r)
	
	for(k in 1:(t-1)){ 
		i<-ind[k]
		j<-ind[k+1]			# In last iteration, j=r
		S[i:(j-1)]<-F0[k+1]
	}
	
	if(procData$rightCensoring){ 
		S[r] <- S[r-1]
	}else{
		S[r] <- 0.0
	}

	## Original data
	originalData <- procData$times[,1]  

	q <- length(originalData)

	ST <- array(0,c(q+1,2))

	ST[1,1] <- 0
	ST[2:(q+1),1] <- originalData

	if(procData$leftCensoring){
		ST[1,2] <- S[1]

		if(procData$leftRepeated){
			ST[2:(q+1),2] <- S[2:(q+1)]
		}else{
			ST[2:(q+1),2] <- S[1:q]
		}
	}
	else{
		ST[1,2] <- 1
		ST[2:(q+1),2] <- S[1:q]
	}


	return(ST)
	
}

