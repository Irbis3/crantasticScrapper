library(recurrentR)

x <- numeric(0)
y <- 0
o <- new(recurrentR:::StepFunction, x, y)
o$call(1)
o$sort_call(0:3)