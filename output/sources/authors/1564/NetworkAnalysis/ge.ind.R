ge.ind <-
function(G){
     n     <- length(degree(G))     
     paths <- as.vector(shortest.paths(G))
     paths[which(paths==0)] <- Inf  # Only for diagonal elements.
     mat <- matrix(paths,nrow=n,ncol=n)
     out <- vector(); for(i in 1:n) out[i] <- sum(1/mat[,i])/(n-1)
return(out)
}

