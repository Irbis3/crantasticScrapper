library(recurrentR)
data(huang_2010)
obj.raw <- attr(huang_2010, "restore")(huang_2010[[2]]) 
obj <- create_recurrent_data(
  matrix(obj.raw$W, ncol=1), y=obj.raw$Y, t=obj.raw$t, 
  W=lapply(obj.raw$X, function(x) {
    force(x)
    function(t) c(x * log(t), x * sin(t))
  }), 10, obj.raw$Y < 10
)

rho.gen <- function(obj, i, k) {
  function(t, u) {
    obj@W[[i]](u) + obj@W[[k]](t) - obj@W[[i]](t) - obj@W[[k]](u)
  }
}

h <- function(obj, i, j, beta) {
#   browser()
  upper_bound <- min(obj@y[i], obj@y[j])
  index_i <- which(obj@t[[i]] < upper_bound)
  index_j <- which(obj@t[[j]] < upper_bound)
  rho <- rho.gen(obj, i, j)
  W.dim <- length(obj@W[[1]](0))
  retval <- rep(0, W.dim)
  for(i1 in index_i) {
    for(i2 in index_j) {
      rho.value <- as.vector(rho(obj@t[[i]][i1], obj@t[[j]][i2]))
      rho.value.beta <- as.vector(rho.value %*% beta)
      retval <- retval - (exp(rho.value.beta) / (1 + exp(rho.value.beta))) * rho.value
    }
  }
  retval
}

h(obj, 1, 2, c(0.5, 0.3))

S <- function(obj, beta) {
  n <- length(obj@y)
  stopifnot(n > 1)
  W.dim <- length(obj@W[[1]](0))
  retval <- rep(0, W.dim)
  for(i in 1:(n-1)) {
    for(j in (i+1):n) {
      retval <- retval + h(obj, i, j, beta)
    }
  }
  retval
}

S(obj, c(0,0))
library(nleqslv)
f <- function(x) S(obj, x)
library(microbenchmark)
microbenchmark(f(c(0,0)))
r <- nleqslv(c(0,0), f)