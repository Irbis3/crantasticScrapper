library(Homework1)

set.seed(2)
## Generate predictor matrix
n <- 100000
p <- 500
X <- cbind(1, matrix(rnorm(n * (p - 1)), n, p - 1))

## Coefficents
b <- rnorm(p)

## Response
y <- X %*% b + rnorm(n)

r <- replicate(5, system.time(fastlm(X, y)))
print(rowMeans(r))
