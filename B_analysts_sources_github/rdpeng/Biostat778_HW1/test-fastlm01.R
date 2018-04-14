library(Homework1)
op <- options(scipen = 5, width = 80)

set.seed(2)
## Generate predictor matrix
n <- 1000
p <- 50
X <- cbind(1, matrix(rnorm(n * (p - 1)), n, p - 1))

## Coefficents
b <- rnorm(p)

## Response
y <- drop(X %*% b + rnorm(n))

fit <- fastlm(X, y)
print(drop(fit$coefficients))
diag(fit$vcov)

options(op)
