library(RPPGen)
library(recurrentR)
library(plotrix)
# set.seed(1)
# lambda <- function(x) exp(sin(x))

do_exp <- function(lambda, Lambda, T_0, gen_z, beta, X, n) {
	curve(lambda, from=0, to = T_0)
	
	y <- rpois(n, T_0)
	y <- as.numeric(ifelse(y < T_0, y, T_0))
	t <- sapply(seq_along(y), function(i) {
		# 	browser()
		z <- gen_z() * exp(as.vector(X[i,] %*% beta))
		lambda_i <- function(t) z * lambda(t)
		retval <- gen_inhomo_poisson(lambda_i, y[i] - 1, lambda_i(0))
		if (is.null(retval)) return(vector("numeric", 0))
		return(retval)
		# 	return(as.numeric(ceiling(retval)))
	})
	obj <- create_recurrent_data(X, y, t, data.frame(), T_0, NULL)
#     new("recurrent-data", X, y, t, data.frame(), T_0)
	
	c.hat.gen <- recurrentR:::c.hat.gen(obj)
	ci <- sapply(1:length(t), c.hat.gen)
# 	obj$Lambda.hat(T_0)
	
	x.eval <- seq(0, obj@T_0, length=100)
	y.eval <- obj$Lambda.hat(x.eval)
	gamma <- obj$U.hat()
	system.time({
		v <- asymptotic.var(obj, gamma = gamma)
		v.eval <- sapply(x.eval, function(x) v$Lambda.hat.var(x))
	})
	u.eval <- y.eval + 2*sqrt(v.eval)
	l.eval <- y.eval - 2*sqrt(v.eval)
	plot(x.eval, Lambda(x.eval), type="l", xlab="x", ylab="", 
		col = 3, lwd = 2, ylim=c(min(l.eval), max(u.eval)))
	lines(x.eval, y.eval, lty=1, col = 1)
	lines(x.eval, l.eval, lty=2)
	lines(x.eval, u.eval, lty=2)
	Lambda.hat <- obj$Lambda.hat
	system.time({
		b.eval <- Lambda.hat(x.eval, bootstrap=TRUE)
	})
	u.eval <- y.eval + 2 * b.eval$error.measurement
	l.eval <- y.eval - 2 * b.eval$error.measurement
	lines(x.eval, u.eval, col=2, lty=2)
	lines(x.eval, l.eval, col=2, lty=2)
	
	m <- 100
	real.sim <- matrix(0, length(x.eval), m)
	pb <- txtProgressBar(style=3, max=m)
	for(i in 1:m) {
		y.sim <- rpois(n, T_0)
		y.sim <- as.numeric(ifelse(y.sim < T_0, y.sim, T_0))
		t.sim <- sapply(seq_along(y.sim), function(i) {
			# 	browser()
			z <- gen_z() * exp(as.vector(X[i,] %*% beta))
			lambda_i <- function(t) z * lambda(t)
			retval <- gen_inhomo_poisson(lambda_i, y.sim[i] - 1, lambda_i(0))
			if (is.null(retval)) return(vector("numeric", 0))
			return(retval)
			# 	return(as.numeric(ceiling(retval)))
		})
		obj.sim <- create_recurrent_data(X, y.sim, t.sim, data.frame(), T_0, NULL)
		real.sim[,i] <- obj.sim$Lambda.hat(x.eval)
		setTxtProgressBar(pb, i)
	}
	close(pb)
	
	lines(x.eval, apply(real.sim, 1, function(a) quantile(a, 0.025)), lty=2, col=3, lwd=2)
	lines(x.eval, apply(real.sim, 1, function(a) quantile(a, 0.975)), lty=2, col=3, lwd=2)
	
	legend("bottomright", 
		c("Lambda.hat", "Lambda", "asymptotic pointwise C.I.", "bootstrap pointwise C.I.", "parametric bootstrap pointwise C.I."), 
		lty=c(1, 1, 2, 2, 2), col=c(1, 3, 1, 2, 3), lwd=c(1, 2, 1, 1, 2))
	par(mfrow=c(1, length(gamma)))
	asymptotic.gamma.sd <- sqrt(diag(v$gamma.var.hat))
	for(i in seq_along(gamma)) {
		plotCI(1, gamma[i], uiw=2 * asymptotic.gamma.sd, 
			xaxt='n', xlab="", ylab = "", main=paste("gamma[", i, "]", sep=""))
		if (i == 1) points(1, log(Lambda(T_0)), col=2) else points(1, beta[i-1], col=2)
	}
}

lambda <- function(x) exp(-x/10)
Lambda <- function(x) 10 * (1 - exp(-x/10))
T_0 <- rpois(1, 40)
gen_z <- function() runif(1, 0.5, 1.5)
n <- 150
beta <- c(1, -1)
X <- cbind(sin(1:n), sample(c(0, 1), n, TRUE))
# mean(X %*% beta)
# sd(X %*% beta)
do_exp(lambda, Lambda, T_0, gen_z, beta, X, n)

lambda <- function(x) rep(1, length(x))
Lambda <- function(x) x
T_0 <- rpois(1, 40)
gen_z <- function() rexp(1)
n <- 100
X <- cbind(
	sample(c(0, 1), n, TRUE), 
	sample(c(0, 1), n, TRUE), 
	sample(c(0, 1), n, TRUE), 
	sample(c(0, 1), n, TRUE))
beta <- rnorm(ncol(X), 0.5)
do_exp(lambda, Lambda, T_0, gen_z, beta, X, n)
# 
# asymptotic.var(obj)