#' Extract Log-Likehood
#'
#' logLik.sglg extracts log-likehood from a model from an object of class 'sglg'.
#' @param object an object of the class sglg. This object is returned from the call to glg(), sglg(), survglg() or ssurvglg() function.
#' @param ... other arguments.
#' @export
logLik.sglg <- function(object, ...) {
    object$llglg
}
