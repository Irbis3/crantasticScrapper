naObj <-
function(obj,D){
  if(D==FALSE){
     if(class(obj)=="igraph"){
         D <- shortest.paths(obj)
     }else{
         g    <- graph.adjacency(obj, mode="undirected",weighted=NULL,diag=FALSE);
         D    <- shortest.paths(g)
    }#obj.
   }else{D <- obj}
return(D)
}

