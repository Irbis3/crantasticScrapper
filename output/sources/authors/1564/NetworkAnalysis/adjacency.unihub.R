adjacency.unihub <-
function(Nv,Ne,graph=FALSE){
    maxE <- Nv*(Nv-1)/2; vec <- vector("numeric",maxE);      
    vec[1:Ne] <- rep(1,Ne)
    A    <- vec2sym2(vec,Nv); for(i in 1:Nv) A[i,i]<-0
    if(graph==TRUE)A <- graph.adjacency(A,mode="undirected",weighted=NULL,diag=FALSE)
return(A)
}

