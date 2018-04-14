module <-
function(beta, gamma){
  c=max(c(abs(mean(beta)),abs(mean(gamma))))
  d=min(c(abs(mean(beta)),abs(mean(gamma))))
  q=d/c
  amp=c*sqrt(1+q*q)
  return(amp)
}
