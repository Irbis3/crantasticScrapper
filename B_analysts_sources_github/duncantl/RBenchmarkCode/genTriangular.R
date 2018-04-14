showTriangular =
function() {
 curve(dtriang(x, 0, 2, 1), 0, 2, lty=1, col="red", ylab="Density")
 curve(dtriang(x, 0, 2, 1.8), 0, 2,  col="blue", add=TRUE)
 curve(dtriang(x, 0, 2, .5), 0, 2, lty=1, col="brown", add=TRUE)
}

dtriang =
  # Density of a triangular distribution.
function(x, a = 0, b = 2, c = 1)
{
 if(!(a <= c && c <= b))
   stop("Incorrect parameters")
  
 ans = rep(0, length(x))

 A = (x < c & x >= a)
 B = (x >= c & x <= b)
 ans[A] = 2*(x[A] - a)/((b-a)*(c-a))
 ans[B] = 2*(b- x[B])/((b-a)*(b - c))

 ans
}  


ctriang =
function(x, a = 0, b = 2, c = 1)
{
 if(!(a <= c && c <= b))
   stop("Incorrect parameters")
 
 ans = rep(0, length(x))

 A = (x < c & x >= a)
 B = (x >= c & x <= b)
 ans[A] = (x[A] - a)^2/((b-a)*(c-a))
 ans[B] = 1 - (b- x[B])^2/((b-a)*(b-c))

 ans
}  

inv.triang =
function(x, a = 0, b = 2, c = 1) {

    if(!(a <= c && c <= b))
      stop("Incorrect parameters")

    ans = rep(0, length(x))
    ans[ x > b] = 1

    cutPoint = (c-a)/(b-a)
    A = (x > a & x < cutPoint)
    B = (x >= cutPoint & x <= b)

    ans[A] = sqrt(x[A]*(b-a)*(c-a)) + a
    ans[B] = b - sqrt((1-x[B])*(b-a)*(b-c)) 

    ans
}

rtriang =
function(n, a = 0, b = 2, c = 1)
{
  u = runif(n)
  inv.triang(u, a, b, c)
}

