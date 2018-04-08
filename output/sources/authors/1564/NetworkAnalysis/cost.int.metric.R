###
### See also cost.int() at the end of
### ../NetworkAnalysis/R/metrics.R
###
###########################

cost.int.metric <-
function(R,
                            bounds=c(0.0,1.0), 
                            Tmc=100,         
                            metric="ge",
                            method="mc",
                            verbose=FALSE,
                            samples=FALSE){  
    # Preliminaries.
    n <- dim(R)[1]; Emax <- n*(n-1)/2; Rvec <- sym2vec(R);
    
    # Bounds standardization to ensure, they refer to actual cost values. 
    bounds <- bound.standard(bounds,Emax); 
    
    # Exhaustive method.    
    if(method=="exhaustive" & Emax<=10000){
       if(metric=="ge"){
         out <- cxx.cost.int(R,metric=0,samples=samples)
       }else{
         g <- 0
         for(e in (bounds[1]*Emax):(bounds[2]*Emax)){ 
            Avec<- cost2adj(Rvec=Rvec,E=e,Emax=Emax)
            A   <- vec2sym(Avec,n)
            G   <- graph.adjacency(A, mode="undirected",weighted=NULL,diag=FALSE);
            g   <- g + top.metric(G,metric)/(bounds[2]*Emax - bounds[1]*Emax+1);
            if(verbose==TRUE){print("############################################")
            print(paste("This is cost: ",e)); print(c(min(Rvec),max(Rvec))); print(ls[[2]])}       
         }# e in Emax
         out <- g
       }# metric.
    }else{method <- "mc"}
    
    # MC method.
    if(method=="mc"){
    vec <- vector("numeric",Tmc); cost <- vector("numeric",Tmc);    
        for(t in 1:Tmc){
            e   <- trunc(runif(1,(bounds[1]*Emax),(bounds[2]*Emax)+1))
            Avec<- cost2adj(Rvec=Rvec,E=e,Emax=Emax)           
            A   <- vec2sym(Avec,n)
            G   <- graph.adjacency(A, mode="undirected",weighted=NULL,diag=FALSE);
            vec[t] <- top.metric(G,metric);             
            cost[t]<- e
        }# t in T    
        # Output.
        if(samples==TRUE){ out <- cbind(vec,cost/Emax)
        }else{ mcMean<- mean(vec); mcSd <- sqrt(sum((vec-mcMean)^2)/Tmc^2); out <- c(mcMean,mcSd) }#fi
    }#fi
return(out)
}# cost.integrated.metric

