#########
vec2sym2 <- function(vec,n){  
   A <- matrix(0,n,n)
   A[1,1] <- 1
   for(i in 1:(n-1)){
       if(i==1){A[i,(i+1):n] <- vec[1:(n-i)]; A[(i+1):n,i] <- A[i,(i+1):n];}
       else{
          #print(i); print((i+1):n); print(sum((n-1):(n-i+1)) + (1:(n-i))); 
          A[i,(i+1):n] <- vec[sum((n-1):(n-i+1)) + (1:(n-i))]; A[(i+1):n,i] <- A[i,(i+1):n];}
       A[i,i] <- 1
   }# i in n
return(A)  
}
