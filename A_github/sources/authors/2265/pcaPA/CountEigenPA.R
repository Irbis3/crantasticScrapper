CountEigen.PA <- function (PA, percentiles = NULL) {
  # # Counts the number of observed eigenvalues that exceed the given percentiles.
  # #
  # # Arg:
  # #  PA: an object of class PA
  # #  percentiles: The percentiles that ought to be plotted, defaults to those in the object
  # #
  # # Ret:
  # #  greaterEigenValues: named numeric vector indicating the number of eigenvalues that are greater than the
  # #  eigenvalues distribution percentiles under independence
 
  ################################################################################
  # # Check object's class
  ################################################################################
  isPA <- Check.PA(PA)
  if (!isPA) {
    stop("PA must be of class PA")
  } 
  
  if (!is.null(percentiles)) {
    PA <- quantile.PA(PA, percentiles = percentiles)
  } 

  greaterEigenValues        <- PA$observed[, "eigenValues"] > PA$percentiles[, "eigenValues"]
  greaterEigenValues        <- by(greaterEigenValues, PA$percentile[, "typeEigenValues"], sum)
  greaterEigenValues        <- as.numeric(greaterEigenValues)
  names(greaterEigenValues) <- paste("p", names(table(PA$percentile[, "typeEigenValues"])), sep = "")
 
  return(greaterEigenValues)
}
