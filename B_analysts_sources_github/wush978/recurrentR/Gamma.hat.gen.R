# options(error=recover)
library(recurrentR)
library(microbenchmark)
data(obj.list)
obj <- obj.list[[1]]
Gamma.hat <- recurrentR:::Gamma.hat.gen(obj)
b <- c(1,2)
microbenchmark(Gamma.hat(b))
