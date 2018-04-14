#' Extract Model Residuals
#'
#' residuals.sglg extracts the deviance-type residuals for a model from an object of class 'sglg'.
#' @param object an object of the class sglg. This object is returned from the call to glg(), sglg(), survglg() or ssurvglg() function.
#' @param ... other arguments.
#' @export
residuals.sglg <- function(object, ...) {
    object$rdev
}
