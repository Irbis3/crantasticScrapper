make_mvn <- function(mean, vcv) {
  logdet <- as.numeric(determinant(vcv, TRUE)$modulus)
  tmp <- length(mean) * log(2 * pi) + logdet
  vcv_i <- solve(vcv)

  counter <- 1L
  function(x) {
    counter <<- counter + 1L
    dx <- x - mean
    -(tmp + rowSums((dx %*% vcv_i) * dx))/2
  }
}

vcv <- matrix(c(1, .6, .25, .6), 2, 2)
lik <- make_mvn(c(0, 0), vcv)

source("slice.R")

res <- mcmc(lik, c(0, 0), 500, c(.05, .05), print_every=10)
environment(lik)$counter
environment(lik)$counter / 500

lik <- make_mvn(c(0, 0), vcv)
res <- mcmc(lik, c(0, 0), 500, c(1, 1), print_every=10)
environment(lik)$counter / 500

w <- diff(apply(res[c(2, 3)], 2, quantile, c(.05, .95)))
lik <- make_mvn(c(0, 0), vcv)
res <- mcmc(lik, c(0, 0), 10000, w, print_every=500)
environment(lik)$counter / 10000

lik <- make_mvn(c(0, 0), vcv)
res <- mcmc(lik, c(0, 0), 10000, c(10, 10), print_every=500)
environment(lik)$counter / 10000

lik <- make_mvn(c(0, 0), vcv)
res <- mcmc(lik, c(0, 0), 10000, 100, print_every=500)
environment(lik)$counter / 10000
