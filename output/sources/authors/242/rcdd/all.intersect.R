allIntersect <- function(sets, pow2) {
    if (! is.list(sets))
        stop("argument must be list")
    if (! all(sapply(sets, storage.mode) == "integer"))
        stop("argument must be list of integer vectors")
    if (missing(pow2))
        pow2 <- ceiling(log2(100 * max(sapply(sets, length))))
    .Call(C_all_intersect, sets, as.integer(pow2))
}
