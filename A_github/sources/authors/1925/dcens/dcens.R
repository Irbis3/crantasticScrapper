dcens <- function(t, d, quantilesST = c(0.25,0.5,0.75),
                        quantilesSL = c(0.25,0.5,0.75),
                        quantilesSR = c(0.25,0.5,0.75), nboot=500){

  if(length(t) != length(d))
	stop("Times and censoring indicators vectors must have the same length.")

  if(any(t < 0))
	stop("Times must be positive.")

  if(any(d < -1 | d > 1))
	stop("Censoring indicators must take values 0, -1 or 1.")

  n <- length(t)
  data <- cbind(t,d)

  
  
  ## Percentage of exact data, left-censored data and right-censored data
  cT <- sum(d == 0)/n
  cL <- sum(d == -1)/n
  cR <- sum(d == 1)/n

  quantilesST <- sort(quantilesST)
  quantilesSL <- sort(quantilesSL)
  quantilesSR <- sort(quantilesSR)

  ## Data processing
  procData <- survProcessing(t,d)
  if(is.null(procData))
	stop("Not enough data.")

  ## Calculation of the ST survival
  ST <- survivalST(procData)

  ## Calculation of the SL survival
  SL <- survivalSL(procData,ST)

  ## Calculation of the SR survival
  SR <- survivalSR(procData,ST)


  ## Calculation of the estimated quantiles
  pcT <- survQuantile(ST,quantilesST) 
  pcL <- survQuantile(SL,quantilesSL) 
  pcR <- survQuantile(SR,quantilesSR)

  
  # Quantiles arrays for the bootstrapping
  pcT.boot <- array(NA,c(nboot,length(quantilesST)))
  pcL.boot <- array(NA,c(nboot,length(quantilesSL)))
  pcR.boot <- array(NA,c(nboot,length(quantilesSR)))

  
  # Bootstrapping
  b <- 0

  while (b < nboot){

	b <- b+1

	## Data sample
	data.boot <- data[sample(1:nrow(data),replace=TRUE),]

	## Data processing
    t.boot <- data.boot[,1]
    d.boot <- data.boot[,2]
	procData.boot <- survProcessing(t.boot,d.boot)

	if(is.null(procData.boot)){
		b <- b-1
		next()
	}

	## Calculation of the ST survival
	ST.boot <- survivalST(procData.boot)

	## Calculation of the SL survival
	SL.boot <- survivalSL(procData.boot,ST.boot)

	## Calculation of the SR survival
	SR.boot <- survivalSR(procData.boot,ST.boot)

	## Calculation of the estimated quantiles
	pcT.boot[b,] <- survQuantile(ST.boot,quantilesST) 
	pcL.boot[b,] <- survQuantile(SL.boot,quantilesSL) 
	pcR.boot[b,] <- survQuantile(SR.boot,quantilesSR) 

  }


  ## Calculation of the estimated quantiles variances	
  vcT <- apply(pcT.boot,2,var,na.rm=TRUE)
  vcL <- apply(pcL.boot,2,var,na.rm=TRUE)
  vcR <- apply(pcR.boot,2,var,na.rm=TRUE)


  ## Results tables
  survivalsTable <- cbind(SL,ST[,2],SR[,2])
  colnames(survivalsTable) <- c("t","SL","ST","SR")

  resultsTableST <- cbind(pcT,ifelse(is.na(pcT),NA,sqrt(vcT)))
  rownames(resultsTableST) <- as.character(quantilesST)
  colnames(resultsTableST) <- c("Estimate","Std. Error")

  resultsTableSL <- cbind(pcL,ifelse(is.na(pcL),NA,sqrt(vcL)))
  rownames(resultsTableSL) <- as.character(quantilesSL)
  colnames(resultsTableSL) <- c("Estimate","Std. Error")

  resultsTableSR <- cbind(pcR,ifelse(is.na(pcR),NA,sqrt(vcR)))
  rownames(resultsTableSR) <- as.character(quantilesSR)
  colnames(resultsTableSR) <- c("Estimate","Std. Error")

  dc <- list(survivalsTable,resultsTableSL,resultsTableST,resultsTableSR,cL,cT,cR)
  names(dc) <- c("survs","quantilesSL","quantilesST","quantilesSR","leftCensored","exactData","rightCensored")

  class(dc) <- "dcens"
  return(dc)

}

