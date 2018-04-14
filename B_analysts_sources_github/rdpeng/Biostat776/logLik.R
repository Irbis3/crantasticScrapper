make.NegLogLik <- function(data) {
        function(mu) {
                -sum(dnorm(data, mean = mu, sd = 1, log = TRUE))
        }
}

set.seed(1)
x <- rnorm(100, 6)
nLL <- make.NegLogLik(x)
nLL
nLL(3)
nLL(5)
nLL(7)

optimize(nLL, c(1, 10))
nLL(1:10)
v <- Vectorize(nLL)
v(1:10)
