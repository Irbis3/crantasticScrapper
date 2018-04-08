d2q <- function(x) {
    if (! is.numeric(x))
        stop("argument must be numeric")
    storage.mode(x) <- "double"
    .Call(C_d2q, x)
}
