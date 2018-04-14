library(Homework1)
op <- options(scipen = 5)

set.seed(30)
p <- 2
n <- 1e6
x <- matrix(rnorm(n * p), n, p)
mu <- rep(0, p)
S <- diag(1, 2)
y <- dmvnorm(x, mu, S, log = TRUE)
summary(y)

options(op)
