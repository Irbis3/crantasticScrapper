library(recurrentR)
data(obj.list)
Sigma.list <- list()
for(i in seq_along(obj.list)) {
  Sigma.list[[i]] <- recurrentR:::Sigma.hat(obj.list[[i]], beta.list[[i]])
  Gamma.hat <- recurrentR:::Gamma.hat.gen(obj.list[[i]])
  beta <- recurrentR:::BorrowStrengthMethod(obj.list[[i]])
  Gamma.hat(beta)
  solve(Gamma.hat(beta)) %*% Sigma.list[[i]] %*% t(solve(Gamma.hat(beta)))
}
