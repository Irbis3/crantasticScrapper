allfaces <- function(hrep) {

    stopifnot(is.character(hrep) || is.numeric(hrep))
    validcdd(hrep, representation = "H")

    if (is.character(hrep)) {
        .Call(C_allfaces, hrep)
    } else {
        storage.mode(hrep) <- "double"
        .Call(C_allfaces_f, hrep)
    }
}
