hastings =
function(x, y, stationary, proposal) {
    min(1, stationary(y)*proposal(x, y)/(stationary(x)*proposal(y, x)))
}


metropolis =
function(x, y, stationary, proposal) {
    min(1, stationary(y)/stationary(x))
}

q.normal =
function(x, x.t, ...) {
  dnorm(x, x.t, ...)
}


mcmc =
  #  r generates a candidate point
  #  q computes the density of the candidate point
  #
  # r = function(x) rnorm(1, x)
  # q = function(x, y) dnorm(x, y, .5)
  #
  # Normal distrbution with Normal proposal.
  # xt = mcmc(-10, r = function(x) rnorm(1, x), q = function(x, y) dnorm(x, y, .1), sta = dnorm, alg = hastings)
  #
  # T distribution using normal proposal.
  # xt = mcmc(-10, r = function(x) rnorm(1, x), q =NULL, sta = function(x) dt(x, 10), alg = metropolis)
  #
  #
function(x.0 = 0, r, q, stationary, n = 1000, algorithm = metropolis)
  {
     xs = numeric(n+1)
     xs[1] = x.0
     for(i in 1:n) {
       y = r(xs[i]) # generate candidate point based on current value
       k = algorithm(xs[i], y, stationary, q) # compute acceptance probability
       xs[i+1] = if(runif(1) <= k) y else xs[i])
       if(is.na(xs[i+1])) 
        stop("missing value generated")
     }

     class(xs) <- "mcmc"
     xs
  }

plot.mcmc <-
  function(x, ...) {
    par(mfrow=c(2,1))

    m = (as.integer(length(x)*.5))    
    hist(x[-(1:m)], main = "MCMC Sample", prob = TRUE,
                    xlab = substitute(paste(X[t], " | ", m < t), list(m = m, n = length(x))))
    
    plot(as.numeric(x))
    lines(x)
  }
