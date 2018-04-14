source("mcmc.R")

# These are very simple-minded MCMCs.
# We are generating data from a univariate Normal and then a t/Student distribution.
# We can of course sample these directly.

xz = mcmc(-10, n = 1e5, r = function(x) rnorm(1, x), q = function(x, y) dnorm(x, y, .5), sta = dnorm, alg = hastings)

xz = structure(xz[ - (1:1000) ], class = class(xz))
qqnorm(xz)

xt = mcmc(-10, n = 1e5, r = function(x) rnorm(1, x), q =NULL, sta = function(x) dt(x, 10), alg = metropolis)
xt = structure(xt[ - (1:1000) ], class = class(xt))

s = seq(0,1, length = length(xt))
qq = qt(s, 10)
plot(qq, sort(xt))
