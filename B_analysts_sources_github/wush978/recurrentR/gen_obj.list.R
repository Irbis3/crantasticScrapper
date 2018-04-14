library(recurrentR)
library(RPPGen)

lambda <- function(x) exp(-x/10)
# par(mfrow=c(1, 2))
# curve(lambda, 0, 40)
Lambda <- function(x) 10 * (1 - exp(-x/10))
# curve(Lambda, 0, 40)
B <- 1000
obj.list <- list()
Z.true.list <- list()
T_0.list <- list()
n <- 100
set.seed(1)
T_0 <- 40
for(i in 1:B) {
  temp <- local({
    
    h <- function(t) rep(1/T_0, length(t))
    gen_z <- function() runif(1, 0.5, 1.5)
    beta <- c(1, -1)
    gamma <- c(2, -3)
    X <- cbind(sin(1:n), sample(c(0, 1), n, TRUE))
    
    y <- rpois(n, T_0)
    y <- as.numeric(ifelse(y < T_0, y, T_0))
    t <- vector("list", n)
    D <- numeric(n)
    Z.true <- sapply(1:n, function(i) gen_z())
    for(i in seq_along(y)) {
      z <- Z.true[i]
      lambda_i <- function(t) z * lambda(t) * exp(as.vector(X[i,] %*% beta))
      # 	h_i <- function(t) z * h(t) * exp(as.vector(X[i,] %*% gamma))
      h_i <- 1/T_0 * (z * Lambda(T_0)) * exp(as.vector(X[i,]%*%gamma))
      D[i] <- rexp(1,  h_i)
      t[[i]] <- gen_inhomo_poisson(lambda_i, min(D[i], y[i]), lambda_i(0))
    }
    stopifnot(all(sapply(t, function(v) ifelse(length(v) > 0, max(v), 0)) < D))
    stopifnot(all(sapply(t, function(v) ifelse(length(v) > 0, max(v), 0)) < y))
    D.index <- D < y
    y[D < y] <- D[D < y]
    list(obj=new("recurrent-data", X, y, t, data.frame(), T_0, D.index), Z.true=Z.true * Lambda(T_0), T_0=T_0)
  })
  obj.list[[i]] <- temp$obj
  Z.true.list[[i]] <- temp$Z.true
}
beta.list <- list()
pb <- txtProgressBar(max=length(obj.list))
for(i in seq_along(obj.list)) {
  obj <- obj.list[[i]]
  beta <- recurrentR:::BSM(obj)
  beta.list[[i]] <- beta
  setTxtProgressBar(pb, i)
}
close(pb)
save(beta.list, obj.list, Z.true.list, file="data/obj.list.rda")
