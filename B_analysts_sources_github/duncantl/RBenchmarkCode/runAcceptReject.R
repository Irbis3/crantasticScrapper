source("nodeDensity.R")

z = expand.grid(0:100, 0:100)
x = nodeDensity(z[, 1], z[, 2])
h = max(x)

a = matrix(x, 101, 101, byrow = TRUE)
contour(0:100, 0:100, a)
persp(0:100, 0:100, a, theta = 25, phi = 57)

rlocations =
function(n, height = 4, accRate = .35)
{
   x = y = numeric(0)  # preallocate or not?
   accept = numeric() # vector of actual acceptance rates
   iter = 0L
   while(length(x) < n) {
       iter  = iter + 1L
         # how many to sample, based on the expected number that we will accept.
       n1 = ceiling((n - length(x))/accRate)

         # Generate locations uniformly
       .x = runif(n1, 0, 100)
       .y = runif(n1, 0, 100)

         # evaluate density at these points.
       f = nodeDensity(.x, .y)
       u = runif(n1, max = height)
       w = u < f

         # Accept thos for which the density exceeds the uniform value.
       x = c(x, .x[w])  # concatenating. 
       y = c(y, .y[w])
       
       accept = c(accept, sum(w)/n1)
         # update acceptance rate estimate. Simple average now.
       accRate = (accRate + sum(w)/n1)/2
   }
   structure(cbind(x, y)[1:n, ], acceptanceRate = accept, iterations = iter)
}


locs = rlocations(10000)
plot(locs[,1], locs[,2], xlim = c(0, 100), ylim = c(0, 100),
       pch = ".", cex = 2, xlab = "X", ylab = "Y",
       main = "Sampled locations")


attributes(locs)
attr(locs, "iterations")
attr(locs, "acceptanceRate")
