
f =
function(x)
{
  y = vector('list', length(x))
  for(i in seq(along = x)) {
     y[[i]] = g(x[[i]])
  }
  y
}

g =
function(x)
{
   2*h(x) + 3
}

h =
function(n)
{
  rnorm(n)
}

