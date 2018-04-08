vec2sym <-
function(vec,Nv=NULL){  
   if(is.null(Nv)){ Nv <-  1/2+sqrt(1+8*length(vec))/2 }
   A <- matrix(0,Nv,Nv)
   A[1,1] <- 1
   for(i in 2:Nv){
       for(j in 1:(i-1)){
             if(i==2){ A[j,i] <- vec[1]; A[i,j] <- vec[1]}
             else{ A[j,i] <- vec[sum(1:(i-2))+j]; A[i,j] <- vec[sum(1:(i-2))+j] }
        }# j in (i-1)
        A[i,i] <- 0
   }# i in n1   
return(A)  
}

