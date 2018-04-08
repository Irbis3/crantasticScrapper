metric.standardization <-
function(R,
                                   type="correlation"){
     if(type=="correlation") R  <- 1-(1-R)/2
     if(type=="hopfield")    R  <- 1-(1-R/(range(R)[2]-range(R)[1]))/2
     # Diagonal values.
     diag(R) <- 1.0
return(R)
}# metric.standardization.

