adjacency.regular <-
function(Nv,Ne,graph=FALSE){
    A <- matrix(0,Nv,Nv);for(i in 1:Nv) A[i,i]<-0   # No tadpoles.
    x <- 1; t <- 1;
    for(e in 1:Ne){      
      A[x,x+t] <- 1; A[x+t,x] <- 1; 
      if(x+t == Nv){x<-0; t<-t+1}
      x <- x+1; 
    }# e in Ne
    if(graph==TRUE)A <- graph.adjacency(A,mode="undirected",weighted=NULL,diag=FALSE)
return(A)
}

