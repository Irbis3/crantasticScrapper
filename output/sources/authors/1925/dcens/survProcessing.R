survProcessing <-
function(t,d){

	## Data ordering
	data <- cbind(t,d)
	dataOrd <- data[order(data[,1],data[,2]),]

	Tord <- dataOrd[,1]
	dord <- dataOrd[,2]

	n <- length(Tord)

	## If first time is right censored, we remove it
	while(dord[1] == 1 && n > 1){
		Tord <- Tord[2:n]
		dord <- dord[2:n]
		n <- n-1
	}

	## If last time is left censored, we remove it
	while(dord[n] == -1 && n > 1){
		Tord <- Tord[1:(n-1)]
		dord <- dord[1:(n-1)]
		n <- n-1
	}

	## Not enough data
	if(n < 2) 
        return(NULL)

	## Initializing lambdas, mus y exact data
	l <- as.numeric(dord == 1)
	m <- as.numeric(dord == -1)
	d <- as.numeric(dord == 0)


	## Generation of vector O with different times (also L, M and D)
	O <- c(Tord[1])
	L <- c(l[1])
	M <- c(m[1])
	D <- c(d[1])

	j <- 1

	for(i in 2:n){
		if(O[j] == Tord[i]){
			M[j] <- M[j]+m[i]
           		L[j] <- L[j]+l[i]
           		D[j] <- D[j]+d[i]
		}else{
			j<-j+1
			O[j] <- Tord[i]
			M[j] <- m[i]
           		L[j] <- l[i]
           		D[j] <- d[i]
		}
	}

	## Not enough data
	if(j < 2) 
        return(NULL)

	r <- j


	## We look whether there is left censoring at the beginning or right censoring at the end
	leftRepeated <- FALSE
	rightRepeated <- FALSE
	leftCensoring <- FALSE
	rightCensoring <- FALSE

	O_ST <- c()
	D_ST <- c()
	M_ST <- c()
	L_ST <- c()

	if(M[1] > 0){ 

		if(M[1]==1 & L[1]==0 & D[1]==0){
			O_ST <- O
			D_ST[1] <- 1
			M_ST[1] <- 0
			L_ST[1] <- 0
			
			D_ST[2:r] <- D[2:r]
			M_ST[2:r] <- M[2:r]
			L_ST[2:r] <- L[2:r]

			leftRepeated <- FALSE

		}
		else{    # Fictitious time
			O_ST[1] <- O[1]/2
			D_ST[1] <- 1
			M_ST[1] <- 0
			L_ST[1] <- 0

			O_ST[2:(r+1)] <- O
			D_ST[2:(r+1)] <- D
			L_ST[2:(r+1)] <- L
			M_ST[2] <- M[1]-1
			M_ST[3:(r+1)] <- M[2:r]

		
			leftRepeated <- TRUE

		}

		leftCensoring <- TRUE
	}
	else{
		
		O_ST <- O
		D_ST <- D
		M_ST <- M
		L_ST <- L

	}

	if(L[r] > 0){ 

		if(L[r]==1 & M[r]==0 & D[r]==0){

			if(leftCensoring & leftRepeated){
				D_ST[r+1] <- 1
				M_ST[r+1] <- 0
				L_ST[r+1] <- 0
			}else{
				D_ST[r] <- 1
				M_ST[r] <- 0
				L_ST[r] <- 0
			}

			rightRepeated <- FALSE
		}
		else{   # Fictitious time
			if(leftCensoring & leftRepeated){	
				O_ST[r+2] <- O[r]+1
				D_ST[r+2] <- 1
				M_ST[r+2] <- 0
				L_ST[r+2] <- 0

				L_ST[r+1] <- L[r]-1
			}
			else{
				O_ST[r+1] <- O[r]+1
				D_ST[r+1] <- 1
				M_ST[r+1] <- 0
				L_ST[r+1] <- 0

				L_ST[r] <- L[r]-1
			}

			rightRepeated <- TRUE

		}

		rightCensoring <- TRUE
	}

	times   <- cbind(O,M,L,D)
	timesST <- cbind(O_ST,M_ST,L_ST,D_ST)

	procData <- list(n,times,timesST,leftCensoring,leftRepeated,rightCensoring,rightRepeated)
	names(procData) <- c("n","times","timesST","leftCensoring","leftRepeated","rightCensoring","rightRepeated")


	return(procData)

}

