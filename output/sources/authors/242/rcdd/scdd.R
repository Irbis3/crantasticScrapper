scdd <- function(input, adjacency = FALSE, inputadjacency = FALSE,
    incidence = FALSE, inputincidence = FALSE, roworder = c("lexmin",
    "maxindex", "minindex", "mincutoff", "maxcutoff", "mixcutoff", "lexmax",
    "randomrow"), keepinput = c("maybe", "TRUE", "FALSE"),
    representation = c("H", "V")) {

    roworder <- match.arg(roworder)
    keepinput <- match.arg(keepinput)
    representation <- match.arg(representation)
    fred <- attr(input, "representation")
    if (! is.null(fred))
        representation <- match.arg(fred, c("H", "V"))
    h <- representation == "H"

    validcdd(input, representation)

    if (is.character(input)) {
        out <- .Call(C_scdd, input, h, roworder, adjacency,
            inputadjacency, incidence, inputincidence)
    } else {
        if (! is.numeric(input))
            stop("input must be numeric or character")
        storage.mode(input) <- "double"
        out <- .Call(C_scdd_f, input, h, roworder, adjacency,
            inputadjacency, incidence, inputincidence)
    }

    attr(out$output, "representation") <- ifelse(! h, "H", "V")

    if (keepinput == "TRUE" | (keepinput == "maybe" &
        (inputadjacency | incidence | inputincidence)))
        out$input <- input
    return(out)
}
