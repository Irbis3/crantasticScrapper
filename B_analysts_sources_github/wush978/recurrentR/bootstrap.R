gen_bootstrap <- function(obj) {
	n <- length(obj@y)
	index_set <- 1:n
	return(function(B) {
		retval <- vector("list", B)
		for(i in seq_along(retval)) {
			index <- sample(index_set, n, TRUE)
			if(nrow(obj@X) > 0) {
				X <- obj@X[index,-1, drop=FALSE]
			} else {
				X <- obj@X
			}
			y <- obj@y[index]
			t <- obj@t[index]
			if(nrow(obj@W) > 0) {
				W <- obj@W[index,]
			} else {
				W <- obj@W
			}
			T_0 <- obj@T_0
			retval[[i]] <- create_recurrent_data(X, y, t, W, T_0, NULL)
		}
		return(retval)
	})
}

