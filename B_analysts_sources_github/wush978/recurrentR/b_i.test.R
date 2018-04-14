library(recurrentR)
library(RPPGen) # library(devtools);install_github('RPPGen', 'wush978')

lambda <- function(x) exp(-x/10)
T_0 <- rpois(1, 40)

curve(lambda, from=0, to = T_0)

y <- rpois(nrow(iris), T_0)
y <- as.numeric(ifelse(y < T_0, y, T_0))
t <- sapply(y, function(y) {
	# 	browser()
	lambda_i <- function(t) exp(rnorm(1)) * lambda(t)
	retval <- gen_inhomo_poisson(lambda_i, y - 1, lambda_i(0))
	if (is.null(retval)) return(vector("numeric", 0))
	return(retval)
	# 	return(as.numeric(ceiling(retval)))
})
obj <- new("recurrent-data", model.matrix(~.,iris), y, t, data.frame(), T_0)
rm(list=c("y", "t", "T_0"), envir=globalenv())



f1 <- recurrentR:::b.hat(obj, 1)
b.hat.gen <- recurrentR:::b.hat.gen(obj)
f2 <- b.hat.gen(1)

x <- seq(0, obj@T_0, length=100)
system.time({
	y1 <- sapply(x, f1)
})
system.time({
	y2 <- sapply(x, f2)	
})
stopifnot(all.equal(y1, y2))
# plot(x, y1, type="l")