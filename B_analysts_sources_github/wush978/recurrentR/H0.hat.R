# options(error=recover)
library(recurrentR)
library(microbenchmark)
data(obj.list)
D.count <- H0.result <- rep(NA, length(obj.list))
for(i in seq_along(obj.list)) {
  obj <- obj.list[[i]]
  H0.hat <- recurrentR:::H0.hat.gen(obj)
  T_0 <- T_0.list[[1]]
  H0.result[i] <- H0.hat(T_0)  
  D.count[i] <- sum(obj@D)
}
# plot(D.count, abs(H0.result - 1))
# mean(H0.result)