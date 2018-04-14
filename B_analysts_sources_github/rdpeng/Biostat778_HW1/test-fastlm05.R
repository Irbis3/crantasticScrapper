library(Homework1)
op <- options(scipen = 5)

set.seed(2)
## Generate predictor matrix
n <- 1000 * 2
p <- 990 * 2
X <- cbind(1, matrix(rnorm(n * (p - 1)), n, p - 1))

## Coefficents
b <- rnorm(p)

## Response
y <- drop(X %*% b + rnorm(n))

fit <- fastlm(X, y)
b.est <- drop(fit$coefficients)
yhat <- drop(X %*% b.est)
summary(yhat)
res <- y - yhat
summary(res)



options(op)
