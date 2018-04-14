library(recurrentR)
n <- 150
a <- list()
y <- rexp(n)
for(i in 1:n) {
	a[[i]] <- runif(rpois(1, 10), 0, y[i])
}

f0 <- function(s) {
	sum(sapply(1:n, function(i) sum(s >= a[[i]] & s <= y[i])))
}


s <- sort(unlist(a))
print("origin")
system.time({
	r1 <- sapply(s, f0)
})
print("eval_N")
system.time({
	y.i <- order(y)
	m <- sapply(a, length)
	r2 <- cumsum(rep(1, length(s))) + recurrentR:::eval_N(s, y[y.i], m[y.i])
})
stopifnot(all.equal(r1, r2))