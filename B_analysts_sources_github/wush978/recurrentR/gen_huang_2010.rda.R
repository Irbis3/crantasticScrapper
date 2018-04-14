library(RPPGen)

gen_z <- function(n = 1) {
  rgamma(n, shape = 1/4, scale = 4)
}

gen_w <- function(n) sample(c(0,1), n, TRUE)

gen_X <- function(n = 1) {
  runif(n)
}

gen_sample <- function(n, lambda_0, param) {
  #   browser()
  Z <- gen_z(n)
  W <- gen_w(n)
  X <- gen_X(n)
  lambda <- vector("list", n)
  for(i in 1:n) {
    lambda[[i]] <- function(t) {
      Z[i] * lambda_0(t) * exp(X[i] * log(t) * param[1] + W[i] * param[2])
    }
  }
  Y <- numeric(n)
  index <- which(W == 1)
  Y[index] <- rexp(length(index), 1/10)
  index <- which(W == 0 & X > 0.5)
  Y[index] <- rexp(length(index), 1/(6 * Z[index] + 4))
  index <- which(W == 0 & X <= 0.5)
  Y[index] <- rexp(length(index), 1/(10 * X[index] + 5))
  Y <- ifelse(Y < 10, Y, 10)
  t <- lapply(1:n, function(i) RPPGen::gen_inhomo_poisson(lambda[[i]], Y[i]))
  list(Z=Z, W=W, X=X, lambda=lambda, Y=Y, t=t, T_0=10)
}

lambda_0 <- list(Vectorize(function(t) 1/2), function(t) sqrt(t)/4)
# c(beta, gamma)
param <- list(c(0, 0), c(0.3, 0), c(0, 0.3), c(0.3, 0.3)) 
n <- c(100, 400)

get_name <- function(i, j, k, l) {
  sprintf("lambda_0:%s param:%s n:%d(%d)", 
          switch(i, "1" = "1/2", "2" = "sqrt(t)/4"),
          sprintf("(%s)", paste(param[[j]], collapse=",")),
          n[k],
          l)
}

huang_2010 <- list()
pb <- txtProgressBar(max = 1000 * length(n) * length(param) * length(lambda_0))
for(i in seq_along(lambda_0)) {
  for(j in seq_along(param)) {
    for(k in seq_along(n)) {
      for(l in 1:1000) {
        temp0 <- gen_sample(n[k], lambda_0[[i]], param[[j]])
        temp1 <- serialize(temp0, connection=NULL)
        temp2 <- memCompress(temp1, "xz")
        huang_2010[[get_name(i,j,k,l)]] <- temp2
        setTxtProgressBar(pb, getTxtProgressBar(pb) + 1)
      }
    }
  }
}
close(pb)
attr(huang_2010, "restore") <- function(raw) unserialize(memDecompress(raw, "xz"))
save(huang_2010, file="huang_2010.rda")

library(RMessenger)
sendXMPPMessage(.google_account, .google_password, to="wush978@gmail.com", message="Huang 2010 is Generated")