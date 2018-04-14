cost2adj <-
function(Rvec,E,Emax,cut=FALSE){
    rk    <- rank(Rvec,ties.method="random")
    cutoff<- Rvec[(rk==(Emax+1-E))]
    Rvec[(rk >= (Emax+1-E))] <- 1.0
    Rvec[(rk <  (Emax+1-E))] <- 0.0
    out   <- Rvec
    if(cut==TRUE){ls <- list(); ls[[1]] <- Rvec; ls[[2]] <- cutoff; out <- ls}
return(out)
}# cost2adj

