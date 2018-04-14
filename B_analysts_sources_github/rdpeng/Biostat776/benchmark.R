library(microbenchmark)

x <- rnorm(500)
y <- 3 + 2*x + rnorm(500)
X <- cbind(1, x)

m <- microbenchmark(
        lm(y ~ x),
        solve(crossprod(X), crossprod(X, y)),
        times = 1000
)

