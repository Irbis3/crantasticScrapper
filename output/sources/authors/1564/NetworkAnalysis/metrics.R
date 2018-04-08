ge <-
function(obj,D=FALSE){
    D    <- naObj(obj,D=D); Dvec <- sym2vec(D); 
    out  <- mean(1/Dvec); 
return(out)
}

le <-
function(G){
      n   <- vcount(G)
      out <- sum(le.ind(G))/n  
return(out)
}

cxx.ge <-
function(D){ 
   out <- .Call("ge",PACKAGE="NetworkAnalysis",as.matrix(D))
return(out)
}

cxx.Hg <-
function(D){
   diag(D) <- rep(0,dim(D)[1])
   out <- .Call("Hg",PACKAGE="NetworkAnalysis",as.matrix(D))
return(out)
}

edge.update <-
function(D,Vpair){ 
   out <- .Call("edge_updateR",PACKAGE="NetworkAnalysis",as.matrix(D),as.integer(Vpair))
return(out)
}

giant.cpnt <-
function(D){ 
   out <- .Call("giant_cpnt",PACKAGE="NetworkAnalysis",as.matrix(D))
return(out)
}

number.cpnts <-
function(D){ 
   out <- .Call("number_cpnts",PACKAGE="NetworkAnalysis",as.matrix(D))
return(out)
}

pos2ind <-
function(pos,Nv){
   out <- .Call("pos2ind",PACKAGE="NetworkAnalysis",as.integer(pos),as.integer(Nv))
return(out)
}

cxx.cost <-
function(obj){
   if(is.igraph(obj)){ out <- ecount(obj)/(vcount(obj)*(vcount(obj)-1)/2)}
   else{ out <- .Call("cost",PACKAGE="NetworkAnalysis",as.matrix(obj)) }
return(out)
}

cxx.order <-
function(x,decreasing){
   out <- .Call("order",PACKAGE="NetworkAnalysis",as.numeric(x),as.integer(decreasing))
   out <- out+1
return(out)
}

cxx.cost.int <-
function(W,metric,samples=FALSE){
   # metric=0 -> E.
   # metric=1 -> H.
   # metric=2 -> E&H.    
   out <- .Call("cost_int",PACKAGE="NetworkAnalysis",as.matrix(W),as.integer(metric))
   if(samples==FALSE) out <- mean(out[,metric+2]) 
return(out)
}

