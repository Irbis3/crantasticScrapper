#'@title Creator of recurrent-data
#'
#'@description Generate the recurrent-data object.
#'
#'@param X data.frame. The time independent covariate of subjects.
#'@param y numeric vector. The censor time.
#'@param t list. The time of recurrent events.
#'@param W list of functions The time dependent covariates.
#'@param T_0 numeric value. The time of termination of experients.
#'@param D numeric vector. The failure time.
#'@export
create_recurrent_data <- function(X, y, t, W, T_0, D) {
  new("recurrent-data", X, y, t, W, T_0, D)
}

#'@title Recurrent Data
#'
#'@description The S4 class for recurrent data.
#'
#'@param X data.frame. The time independent covariate of subjects.
#'@param y numeric vector. The censor time.
#'@param t list. The time of recurrent events.
#'@param W data.frame. The time dependent covariates.
#'@param T_0 numeric value. The time of termination of experients.
#'@param D numeric vector. The failure time.
setClass(
	"recurrent-data",
	representation(
		X = "matrix",
		y = "numeric",
		t = "list",
		W = "list",
		T_0 = "numeric",
		D = "logical",
		B = "list"
	)
)

setMethod(
	"initialize", 
	"recurrent-data",
	function(.Object, X, y, t, W, T_0, D) {
		if (ncol(X) > 0) .Object@X <- cbind(1, X) else .Object@X <- X
		.Object@y <- y
		.Object@t <- t
		.Object@W <- W
		.Object@T_0 <- T_0
		if (length(D) == 0) {
			.Object@D <- rep(FALSE, length(y))
		} else {
			.Object@D <- D
		}
		.Object@B <- list()
		.Object
	}
	)


setMethod(
	"$",
	signature(x = "recurrent-data"),
	function (x, name) {
		switch(name,
			"F.hat" = F.hat(x),
			"Lambda.hat" = Lambda.hat(x),
			"bootstrap" = gen_bootstrap(x),
			"U.hat" = U.hat(x)
		)
	}
)

#'@exportMethod curve
setMethod("curve",
					signature(expr = "recurrent-data"),
					function (expr, from = NULL, to = NULL, n = 101, add = FALSE, 
										type = c("F.hat", "Lambda.hat"), xname = "x", xlab = xname, ylab = NULL, log = NULL, 
										xlim = NULL, bootstrap = FALSE, B = 100, error.measurement.function = stats::sd, 
										bootstrap.lty = 2, bootstrap.col = 1, bootstrap.u = function(result) {
											result$estimate + 2 * result$error.measurement	
										}, bootstrap.l = function(result) { 
											result$estimate - 2 * result$error.measurement
										}, ...) 
					{
						if (is.null(from)) from <- 0
						if (is.null(to)) to <- expr@T_0
						switch(type, 
							"F.hat" = {
								F.hat <- expr$F.hat
								curve(F.hat, from, to, n, add, type="l", xname = xname, xlab = xlab, ylab = ylab, 
									log = log, xlim = xlim, ...)
								if (bootstrap) {
									x.eval <- seq(from, to, length.out=n)
									result <- expr$F.hat(x.eval, bootstrap, B=B, error.measurement.function=error.measurement.function)
									lines(x.eval, bootstrap.u(result), lty = bootstrap.lty, col = bootstrap.col)
									lines(x.eval, bootstrap.l(result), lty = bootstrap.lty, col = bootstrap.col)
								}
							},
							"Lambda.hat" = {
								Lambda.hat <- expr$Lambda.hat
								curve(Lambda.hat, from, to, n, add, type="l", xname = xname, xlab = xlab, ylab = ylab, 
									log = log, xlim = xlim, ...)
								if (bootstrap) {
									x.eval <- seq(from, to, length.out=n)
									result <- expr$Lambda.hat(x.eval, bootstrap, B=B, error.measurement.function=error.measurement.function)
									lines(x.eval, bootstrap.u(result), lty = bootstrap.lty, col = bootstrap.col)
									lines(x.eval, bootstrap.l(result), lty = bootstrap.lty, col = bootstrap.col)
								}
							}
						)
					}
)

# #'@exportMethod plot
# setMethod("plot",
# 					signature(x = "recurrent-data"),
# 					function (x, y, type=c("U.hat"), bootstrap = FALSE, B = 100, error.measurement.function = stats::sd, 
# 										bootstrap.lty = 2, bootstrap.col = 1, bootstrap.u = function(result) {
# 											result$estimate + 2 * result$error.measurement	
# 										}, bootstrap.l = function(result) { 
# 											result$estimate - 2 * result$error.measurement
# 										}, tol = 1e-04, ...) 
# 					{
# 						browser()
# 						solver <- x$U.hat
# 						U.hat <- solver(bootstrap=bootstrap, B=B, error.measurement.function=error.measurement.function, tol=tol)
# 						if (bootstrap) {
# 						}	else {
# 							names(U.hat) <- colnames(obj@X)
# 							
# 						}
# 					}
# )
