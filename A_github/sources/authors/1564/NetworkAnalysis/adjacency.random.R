adjacency.random <-
function(Nv,Ne,graph=FALSE){
    if(Ne<=0) stop("Incorrect value for Ne.")
    maxE <- Nv*(Nv-1)/2;

    # Initialize:
    Omega0 <- 1:maxE; Omega1 <- NULL
      
    # Sample:
    for(t in 1:Ne){
        set    <- setdiff(Omega0,Omega1); N <- length(set)
        omega  <- set[trunc(runif(1,1,N+1))]
        Omega1 <- c(Omega1,omega)
    }# i
    Avec <- vector("numeric",maxE); Avec[Omega1] <- 1
    A <- vec2sym(Avec,Nv); for(i in 1:Nv) A[i,i]<-0
    if(graph==TRUE)A <- graph.adjacency(A,mode="undirected",weighted=NULL,diag=FALSE)
return(A)
}

