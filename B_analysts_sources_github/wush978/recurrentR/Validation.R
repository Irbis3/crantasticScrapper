library(recurrentR)
library(microbenchmark)
library(numDeriv)

data(obj.list)
obj <- obj.list[[1]]
U.hat <- recurrentR:::U.hat.gen(obj)
Gamma.hat <- recurrentR:::Gamma.hat.gen(obj)
b <- 1:2
U.hat(b)
jacobian(U.hat, b)
abs(Gamma.hat(b) - jacobian(U.hat, b))
microbenchmark(Gamma.hat(b))