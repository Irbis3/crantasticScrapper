qneg <- function(x) {

    if (! (is.numeric(x) || is.character(x)))
        stop("'x' not numeric or character")

    if (is.numeric(x))
        x <- d2q(x)

    .Call(C_qo, x, as.integer(1))
}
