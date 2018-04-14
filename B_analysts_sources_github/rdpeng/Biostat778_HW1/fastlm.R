fastlm <- function(X, y, na.rm = FALSE) {
        if(na.rm) {
                u <- complete.cases(X, y)
                X <- X[u, ]
                y <- y[u]
        }
        XtX <- crossprod(X)
        ch <- chol(XtX)
        V <- chol2inv(ch)
        b <- drop(V %*% crossprod(X, y))
        res <- drop(y - X %*% b)
        sigma2 <- sum(res * res) / (length(y) - ncol(X))
        list(coefficients = b, vcov = sigma2 * V)
}

