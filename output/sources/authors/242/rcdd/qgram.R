
# rational gram schmidt

qgram <- function(x, remove.zero.vectors = TRUE) {

    if (! (is.numeric(x) || is.character(x)))
        stop("'x' not numeric or character")
    if (is.numeric(x))
        x <- d2q(x)
    if (! (is.character(x))) stop("Cannot happen!")
    if (! (is.matrix(x)))
        stop("'x' must be matrix")
    stopifnot(is.logical(remove.zero.vectors))
    stopifnot(length(remove.zero.vectors) == 1)

    result <- .Call(C_qgram, x)

    if (remove.zero.vectors) {
        is.zero <- apply(result == "0", 2, all)
        return(result[ , ! is.zero, drop = FALSE])
    } else {
        return(result)
    }
}

