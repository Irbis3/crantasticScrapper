dnorm =
function(x, mean = 0, sd = 1)
  exp( -.5 * ((x - mean)/sd)^2)/ (sd * sqrt(2*pi))


rnorm =
  # Box-Mueller method for generating normals.
function(n, mean = 0, sd = 1)
{
  n1 = ceiling(n/2)
  u = runif(n1, 0, 1)
  v = runif(n1, 0, 1)
  k = sqrt(-2 * log(u))
  k2 = 2 * pi * v
  k*c(cos(k2), sin(k2))[1:n]
}
