f = function(x) { return(x) }

g = function(x) { x }

h = function(x) {
  if(length(x) == 0)
    return(NULL)
  else if(all(is.na(x)))
    return(character(1))

  return(sapply(x, as.character))
}

j = function(x) {
  if(length(x) == 0)
    return(NULL)
  else if(all(is.na(x)))
    return(character(1))
}

For = function(x) {
  for(i in seq(along = x)[-1]) {
     if(x[i] > x[i-1])
        return(TRUE)
  }
}

While = function(x) {
  ctr = 0
  while(TRUE) {
     if(ctr > 10)
        return("not converged")
     ctr = ctr + 1
  }
  mtcars
}

branch =
function(x)
{
   if(x < 10)
       abc(x)
   else
       def(x)
}

abc = function(x) x
def = function(x) -x

top =
  #
  # branch = provenanceTrace(branch)
  # top()
function()
{
   lapply(rnorm(10, 100), function(x) branch(x))
}


npBootstrap =
function(expr, data, B = 999)
{
  e = new.env(parent = globalenv())
  replicate(B, {
              i = sample(1:nrow(data), nrow(data), replace = TRUE)
              e$data = data[i,]
              eval(expr, i)
            })
}
