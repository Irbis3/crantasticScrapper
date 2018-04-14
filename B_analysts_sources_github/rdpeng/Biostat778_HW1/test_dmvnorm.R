library(Homework1)

## Create the covariance matrix
n <- 100
n2 <- n^2
xg <- seq(0, 1, length = n)
yg <- xg
g <- data.matrix(expand.grid(xg, yg))
D <- as.matrix(dist(g))
phi <- 5

S <- exp(-phi * D)
mu <- rep(0, n2)
set.seed(1)
x <- matrix(rnorm(n2), byrow = TRUE, ncol = n2)

r <- replicate(5, system.time(dmvnorm(x, mu, S, log = TRUE)))
print(rowMeans(r))
