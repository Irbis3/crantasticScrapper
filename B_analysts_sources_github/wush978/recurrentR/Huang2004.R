library(recurrentR)

obj <- local({
  n <- 100
  x <- sample(0:1, n, TRUE)
  T_0 <- 40
  y <- rexp(n, 1/T_0)
  D <- y < T_0
  y <- ifelse(D, y, T_0)
  t <- lapply(y, function(y) {
    n <- rpois(1, y * 0.4)
    if (n == 0) return(numeric(0))
    sort(runif(n, 0, y))
  })
  create_recurrent_data.numeric(y, D, t, T_0, matrix(x, ncol = 1))
})

U <- recurrentR:::U.gen(obj)
Gamma <- recurrentR:::Gamma.gen(obj)
if (require(numDeriv)) {
  alpha <- rnorm(ncol(obj@W))
  r1 <- jacobian(U, alpha)
  r2 <- Gamma(alpha)
  attr(r2, "dimnames") <- NULL
  stopifnot(all.equal(r1, r2))
}
