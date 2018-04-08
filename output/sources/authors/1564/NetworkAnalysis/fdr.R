fdr <-
function(pVec,
                alpha=.05){
  
    n    <- length(pVec);
    rVec <- rank(pVec);
    out  <- vector("numeric",n)
    for(i in 1:n){
       if(pVec[i]<=alpha*rVec[i]/n){out[i] <- 1}else{out[i] <- 0}
    }# i
return(out)
}

