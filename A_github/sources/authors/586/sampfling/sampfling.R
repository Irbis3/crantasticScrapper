# This is the R code for the "sampfling" package

sampfle <- function(x, size, prob=NULL)
{
  if(length(x) == 1 && x >= 1) 
    .External("sampfle", as.integer(x), as.integer(size),
              as.double(prob), PACKAGE="sampfling")
  else 
    x[.External("sampfle", as.integer(length(x)), as.integer(size),
                as.double(prob), PACKAGE="sampfling")]
}

samprop <- function(x, size, prob=NULL)
  {
    single <- length(x)==1 && x>=1
    L <- ifelse(single, x, length(x))
    prob <- prob/max(prob)
    z <- function(c) c*prob/(1+c*size*prob)
    z.prob <- function(c) sum(z(c))-1
    limit <- 2
    while (z.prob(limit) < 0)
      limit <- 2 * limit
    prob <- z(uniroot(z.prob, c(0,limit))$root)
    correction <- function(s) 1/(1-sum(prob[s]))
    bound <- correction(order(prob)[(L-size+1):L])
    repeat
      {
        s <- .External("sampfle", as.integer(L), as.integer(size),
                       as.double(prob), PACKAGE="sampfling")
        if (runif(1,0,bound) < correction(s)) break
      }
    if (single)
      s
    else
      x[s]
  }
    
