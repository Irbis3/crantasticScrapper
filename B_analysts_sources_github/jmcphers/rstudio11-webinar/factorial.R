
# Compute the factorial of a number
factorial <- function(x) {
  if (x <= 1)
    1
  else
    x*factorial(x-1)
}
