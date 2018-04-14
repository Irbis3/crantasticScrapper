# options(error=recover)
library(recurrentR)
library(microbenchmark)
data(obj.list)
obj <- obj.list[[1]]
U.hat <- recurrentR:::U.hat.gen(obj)
# debug(U.hat)
b <- rnorm(2)
microbenchmark(U.hat(b))
