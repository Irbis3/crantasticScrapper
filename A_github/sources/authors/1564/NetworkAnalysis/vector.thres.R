vector.thres <-
function(Rvec,C){
     Avec <- vector("numeric",length(Rvec))
     ind.gre  <- which(Rvec>C); ind.low  <- which(Rvec<=C)
     Avec[ind.gre] <- 1; Avec[ind.low] <- 0     
return(Avec)
}# vector.thres

