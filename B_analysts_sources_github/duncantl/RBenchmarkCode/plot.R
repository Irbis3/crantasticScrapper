plot.RobotLog =
function(x, col = makeColorRamp(nrow(x)), ...)
{
 plot(y ~ x,  x, type = "p", pch = 20, col = col, ...)
 points(x[c(1, nrow(x)), "x"],  x[c(1, nrow(x)), "y"],
          pch = c("X", "+"), col = c("blue", "red"), cex = 2)
}

makeColorRamp =
function(n)
{
  s = (1:n)/n
  zero = rep(0, n)
  rgb(zero, s, zero)
}



plotLook =
function(row, ...)
{
   x = row[1, "x"]
   y = row[1, "y"]

  theta = seq(0, 2*pi, length = 360)
  r = as.numeric(row[1, -c(1:4, 365)])
  x1 = x + r*cos(theta)
  y1 = y + r*sin(theta)
  plot(x + 2*cos(theta), y + 2*sin(theta), col = "green", type = "l", xlab = "x", ylab = "y", ...)    
  points(x1, y1, type = "l")
  points(x, y, col = "green")
}

showFinalLook =
function(log, ...)
{
  plotLook(log[nrow(log), ], ...)
}
