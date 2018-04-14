createGrid = 
function(dims = c(100, 100), numCars = .3)
{
   if(length(dims) == 1)
     dims = rep(dims, 2)

   if(length(numCars) == 1 && numCars < 1)
      numCars = rep(prod(dims) * numCars/2, 2)

   grid = matrix("", dims[1], dims[2])

   pos = sample(1:prod(dims), sum(numCars))
   grid[pos] = sample(rep(c("red", "blue"), ceiling(numCars)))[seq(along = pos)]

   structure(grid, class = c("BMLGrid", "matrix"))
}


plot.BMLGrid =
function(x, ...)
{
   if(typeof(x) == "character")
     z = matrix(match(x, c("", "red", "blue")), nrow(x), ncol(x))
   else
     z = x
   image(t(z)[, nrow(z):1], col = c("white", "red", "blue"),
          axes = FALSE, xlab = "", ylab = "", ...)
   box()
}



runBML = 
function(grid = createGrid(...), numSteps = 100, ...)
{
  for(i in 1:numSteps) {
    grid = moveCars(grid, "red")
    grid = moveCars(grid, "blue")
  }

  grid
}

