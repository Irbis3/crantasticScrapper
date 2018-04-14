
nodeDensity =
  # x is a numeric vector 
  # y is a numeric vector the same length as x
  # z is returned. 
  # It is a numeric vector that provides the value of the node density function
function (x, y, a = 3, b = 1) {

      # Check the inputs for the correct format
   if (mode(x) != "numeric" | mode(y) != "numeric")
      stop("x and y must be numeric")
   if (length(x) != length(y))
       stop("x and y must be same length")

  a = 3
  b = 1
  band = 15 
  bank = 1
  inBoundary = (0 <= x & x <= 100) & 
               (0 <= y & y <= 100 & y < sqrt(110^2 - x^2))

  river = abs(sqrt(x^2 + y^2) - 60) 
  hiArea = river> bank & river < band & inBoundary

  hiDensity = a * cos(river[hiArea] * pi / (2 * band)) + b 

  z = b * inBoundary
  z[hiArea] = hiDensity
  z[river <= bank] = 0
   
  z
}
