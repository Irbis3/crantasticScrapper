le.ind <-
function(G){  
     out <- vector()
     n   <- length(degree(G))
     
     # Compute subgraph for every node.
     # BEWARE: Neighors start at 0.
     for(v in 0:(n-1)){
         subG  <- subgraph(G,neighbors(G,v))
         paths <- shortest.paths(subG)
         paths <- as.vector(paths)
         paths[which(paths==0)] <- Inf
         subV  <- vcount(subG)         
         out[v+1] <- sum(1/paths)*(subV*(subV-1))^(-1)
         if(subV==1 | subV==0) out[v+1] <- 0.0
     }# i in (n-1)      
return(out)
}

