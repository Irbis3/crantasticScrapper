library(recurrentR)
set.seed(1)
	
x <- 1:10
y <- rnorm(11)

f1 <- new(recurrentR:::StepFunction, x, y)
f2 <- stepfun(x, y)

for(i in 0:11) {
	stopifnot(f1$call(i) == f2(i))
	stopifnot(f1$call(i + 0.5) == f2(i + 0.5))
}


x <- 0:11
stopifnot(f1$sort_call( x ) == f2(x))
f1$sort_call(x)^2
f1$sort_call(x)/2
(f1^2)$sort_call(x)
stopifnot((f1^2)$sort_call( x ) == f1$sort_call( x )^2)

x <- sort(rexp(100))
a <- quantile(x, 0.25) + 100
b <- quantile(x, 0.75) - 100
i1 <- which(x >= a & x <= b)
i2 <- .Call("substring_index", x, a, b)
if (length(i1) == 0) {
  stopifnot(length(i2) == 0)
} else {
  all.equal(i1, i2)
}