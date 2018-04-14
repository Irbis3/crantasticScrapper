#'@title Integration on step function
#'
#'@param g Function, the integrand
#'@param f Step function, the integrator
#'@param a Numeric value
#'@param b Numeric value
#'
#'@description 
#'Evaluate \deqn{\int_a^b {g(x) df(x)}} where \eqn{f} is a step function
#'
#'@return Numeric value
#'
#'@export
step_integrate <- function(g, f, a, b) {
	stopifnot("stepfun" %in% class(f))
	e <- environment(f)
	x <- e[["x"]]
	y <- c(e[["f"]], e[["y"]])
	p <- diff(y)
	index <- which(x >= a & x <= b)
	if (length(index) == 0) return(0)
	sum(g(x[index]) * p[index])
}

#'@export
step_integrate.StepFunction <- function(g, f, a, b) {
	x <- f$x
# 	index <- which(x >= a & x <= b)
	index <- .Call("substring_index", x, a, b)
	if (length(index) == 0) return(0)
	y <- f$y
	p <- diff(y)
	sum(.Call("StepFunction_sort_call", g, x[index], PACKAGE="recurrentR") * p[index])
}
