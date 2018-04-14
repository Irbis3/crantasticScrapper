## Check `log = FALSE'

library(Homework1)
op <- options(scipen = 5)

mu <- rep(0, 100)
S <- diag(1, 100)
x <- rep(0, 100)
y <- dmvnorm(x, mu, S, log = TRUE)
print(y, digits = 10)

x <- rep(1, 100)
y <- dmvnorm(x, mu, S, log = TRUE)
print(y, digits = 10)

options(op)
