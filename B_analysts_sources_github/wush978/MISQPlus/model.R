{ # generator and moment
  generator <- function(size) rnorm(size)
  density <- function(x) dnorm(x)
  density.lower <- -Inf
  density.upper <- Inf
  M_1 <- integrate( function(x) density(x)*x, density.lower, density.upper )
  M_2 <- integrate( function(x) density(x)*x^2, density.lower, density.upper )  
  M_3 <- integrate( function(x) density(x)*x^3, density.lower, density.upper )  
  M_4 <- integrate( function(x) density(x)*x^4, density.lower, density.upper )  
}
save.image(file="model.Rdata")
