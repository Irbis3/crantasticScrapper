naDensity <-
function(R){
    n   <- dim(R)[1]; 
    out <- sum(sym2vec(R))/(n*(n-1)/2)
return(out)
}

