na.DistanceOrdering <-
function(Dg,Dh){
   if(sum(dim(Dg)!=dim(Dh))>0) stop("Dg and Dh do not identical dimensions.")
   out <- .Call("distance_ordering",PACKAGE="NetworkAnalysis",as.matrix(Dg),as.matrix(Dh))
return(out)
}

na.IndirectPaths <-
function(W,Vpair,k,output="Summaries"){
   if(k==0) stop("Order of indirect paths should not lesser than 1.")
   Vpair <- Vpair-1; 
   out <- .Call("indirect_paths",PACKAGE="NetworkAnalysis",as.matrix(W),as.integer(Vpair),as.integer(k))
   
   # Return Mean, Mode, Sum and variance.
   if(output=="Summaries"){
     vec <- out
     out <- vector("numeric",4);
     out[1] <- mean(vec);
     out[2] <- max(vec);
     out[3] <- sum(vec);
     out[4] <- sd(vec); 
   }#Sum.
return(out)
}

