transposing <- function(R){
  n <- dim(R)[1]
  A <- matrix(0,n,n)
  for(i in 1:n) for(j in 1:n) A[i,j] <- R[i,(n-j+1)]; 
return(A)
}
