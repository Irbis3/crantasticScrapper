coef.PA <- function (object, ...) {
  # # Extracts the observed eigenvalues and the percentiles as a matrix
  # #
  # # Arg:
  # #  PA: an object of class PA
  # #
  # # Ret:
  # #  coef: the matrix with the observed eigenvalues and the percentiles
  
  ################################################################################
  # # Check object's class
  ################################################################################
  isPA <- Check.PA(object)
  if (!isPA) {
    stop("object must be of class PA")
  } 
  
  levels(object$observed[, "typeEigenValues"]) <- levels(object$percentiles[, "typeEigenValues"]) <- 
    unique(c(levels(factor(object$observed[, "typeEigenValues"])), names(table(object$percentiles[, "typeEigenValues"]))))
  PA <- rbind(object$observed, object$percentiles)

  wideEigenValues <- reshape(PA, direction = "wide", timevar = "typeEigenValues", idvar = "orderEigenValues")
  rownames(wideEigenValues) <- wideEigenValues[, "orderEigenValues"]
  colnames(wideEigenValues) <- gsub("eigenValues.", "", colnames(wideEigenValues))
  colnames(wideEigenValues) <- gsub("^(\\d+)", "p\\1", colnames(wideEigenValues))
  wideEigenValues <- as.matrix(wideEigenValues[, -1])

  return(wideEigenValues)
}
