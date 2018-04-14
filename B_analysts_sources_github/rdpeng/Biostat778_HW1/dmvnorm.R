## 'S' must be symmetric p x p
## 'x' is n x p
## 'm' is p x 1
dmvnorm <- function(x, mu, S, log = TRUE) {
        if(is.null(dim(x)))
                x <- matrix(x, byrow = TRUE, ncol = length(x))
        k <- ncol(x)
        d <- sweep(x, 2, mu, "-", check.margin = FALSE)
        R <- try(chol(S), silent = TRUE)
        if(inherits(R, "try-error"))
                stop("S is not positive definite")
        z <- backsolve(R, t(d), transpose = TRUE)
        distval <- colSums(z * z)
        logdet <- 2 * sum(log(diag(R)))
        r <- -(k * log(2 * pi) + logdet + distval) / 2
        if(log)
                r
        else
                exp(r)
}

dmvnorm1 <- function(x, m, S, log = TRUE) {
        if(is.null(dim(x)))
                x <- matrix(x, byrow = TRUE, ncol = length(x))
        k <- ncol(x)
        d <- sweep(x, 2, m, "-")
        R <- suppressWarnings(chol(S, pivot = TRUE))
        if(attr(R, "rank") != k)
                stop("S is not positive definite")
        pivot <- attr(R, "pivot")
        z <- backsolve(R, t(d[, pivot, drop = FALSE]), transpose = TRUE)
        distval <- colSums(z * z)
        logdet <- 2 * sum(log(diag(R)))
        r <- -(k * log(2 * pi) + logdet + distval) / 2
        if(log)
                r
        else
                exp(r)
}

## The slow way
dmvnorm0 <- function(x, mu, S, log = TRUE) {
        k <- NCOL(x)
        logdet <- determinant(S, logarithm = TRUE)$modulus
        distval <- mahalanobis(x, mu, S)
        ret <- -(k * log(2 * pi) + logdet + distval) / 2
        if(log)
                ret
        else
                exp(ret)
}
