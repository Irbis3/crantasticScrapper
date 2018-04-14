library(recurrentR)

obj <- local({
  n <- 50
  #   n <- 400
  x1 <- sample(0:1, n, TRUE)
  x2 <- sample(0:1, n, TRUE)
  T_0 <- 40
  y <- rexp(n, 1/T_0)
  D <- y < T_0
  y <- ifelse(D, y, T_0)
  t <- lapply(y, function(y) {
    n <- rpois(1, y * 0.4)
    if (n == 0) return(numeric(0))
    sort(runif(n, 0, y))
  })
  X <- lapply(1:n, function(i) {
    force(i)
    function(x) c(sin(i * x), cos(i * x))
  })
  create_recurrent_data.list(X, y, D, t, T_0, cbind(x1, x2))
})

obj@cache$beta.hat <- c(0,0)
R_beta <- recurrentR:::R_beta.gen(obj)
N <- local({
  s <- obj@s
  d <- obj@d
  N <- cumsum(d)
  y.i <- order(obj@y)
  m <- sapply(obj@t, length)
  N + recurrentR:::eval_N(s, obj@y[y.i], m[y.i])
})
# stopifnot(R_beta == N)
