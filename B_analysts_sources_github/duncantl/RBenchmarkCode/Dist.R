Dist <-
  #
  # Compute the distance between two sets of observations
  #
  # Compile for numeric and integer inputs. 
  #  (Combinations of both?)
  #
  # We would like to use apply() so that we can parallelize
  # or outer(1:nrow(g1), 1:nrow(g2), f) to avoid building the
  # grid first but fusing the loops.
  #
function(g1, g2, op = euclidean, ...)
{
  ans <- matrix(0, nrow(g1), nrow(g2))

  for(i in seq(length = nrow(g1)))
    for(j in seq(length = nrow(g2)))
       ans[i,j] <- op(g1[i,], g2[j,], ...)

  ans
}



euclidean <-
function(x, y)
   sqrt(sum((x - y)^2))


manhattan <-
function(x, y)
  sum(abs(x - y))

maximum <-
function(x, y)
  max(abs(x - y))


canberra <-
function(x, y)
   sum(abs(x - y)/abs(x + y))


minkowski <-
function(x, y, p = 2)
   (sum( abs(x - y)^p))^(1/p)

binary <-
function(x, y)
{
   w =  x != 0 | y != 0
   sum((x[w] != 0 & y[w] != 0) == 1)/sum(w)
}
