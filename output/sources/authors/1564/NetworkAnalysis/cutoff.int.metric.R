cutoff.int.metric <-
function(R,
                            bounds=c(0.0,1.0), 
                            Tmc=100,                                   
                            metric="ge",
                            method="mc",
                            samples=FALSE){  
    # Preliminaries.
    n <- dim(R)[1]; Emax <- n*(n-1)/2; Rvec <- sym2vec(R); 
    
    # Exhaustive method.
    if(method=="exhaustive" & Emax<=10000){
         # Collect data points.
         dat <- matrix(0,Emax,2);
         bounds <- bound.standard(bounds,Emax); 
         for(e in (bounds[1]*Emax):(bounds[2]*Emax)){ 
            ls  <- cost2adj(Rvec=Rvec,E=Emax-e+1,Emax=Emax,cut=TRUE); # 1.Check that cut value is correct!
            Avec<- ls[[1]]; 
            A   <- vec2sym(Avec,n);
            G   <- graph.adjacency(A, mode="undirected",weighted=NULL,diag=FALSE);
            dat[e,] <- c(top.metric(G,metric),ls[[2]])
         }# e in Emax
         # Compile results (Riemannian summation)
         for(l in 1:Emax){
             if(l==1) out <- dat[1,1]*dat[1,2]  # 2.Check that cut value is correct!
             else     out <- out + dat[l,1]*abs(dat[l,2] - dat[l-1,2])
         }# l in Emax.
         # Beware: Quite different if we use a subset of the Codomain of C.
         
    }else{method <- "mc"}
    # MC method.
    if(method=="mc"){
    vec <- vector("numeric",Tmc); cut <- vector("numeric",Tmc);    
        for(t in 1:Tmc){
            c   <- runif(1,bounds[1],bounds[2])  
            Avec<- vector.thres(Rvec,c)           
            A   <- vec2sym(Avec,n)
            G   <- graph.adjacency(A, mode="undirected",weighted=NULL,diag=FALSE);
            vec[t] <- top.metric(G,metric);
            cut[t] <- naCost(G)
        }# t in T
        # Output.
        if(samples==TRUE){ out <- cbind(vec,cut)
        }else{ mcMean<- mean(vec); mcSd <- sqrt(sum((vec-mcMean)^2)/Tmc^2); out <- c(mcMean,mcSd) }#fi
    }#fi
return(out)
}# real.integrated.metric

