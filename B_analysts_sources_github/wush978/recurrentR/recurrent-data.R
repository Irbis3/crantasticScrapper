library(recurrentR)

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
obj <- create_recurrent_data.numeric(y, D, t, T_0, matrix(x, ncol = 1))
obj <- try(create_recurrent_data.numeric(y, D, t, T_0, matrix(x, ncol = 2)), silent=TRUE)
stopifnot(class(obj) == "try-error")

obj <- create_recurrent_data(y, D, t, T_0, matrix(x, ncol = 1))
X <- lapply(1:n, function(x) function(t) x * t)
obj <- create_recurrent_data(X, y, D, t, T_0, matrix(x, ncol = 1))
X <- X[1:(n-1)]
obj <- try(create_recurrent_data(X, y, D, t, T_0, matrix(x, ncol = 1)), silent=TRUE)
stopifnot(class(obj) == "try-error")

gen_obj <- function(n) {
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
}

obj.list <- lapply(seq(100, by=100, length.out=10), gen_obj)
test <- sapply(obj.list, function(obj) identical(obj@cache, obj.list[[1]]@cache))
stopifnot(all(!tail(test, -1)))
