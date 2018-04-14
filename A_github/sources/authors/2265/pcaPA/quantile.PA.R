quantile.PA <- function (x, percentiles = .99, ...) {
  # # Calculates arbitraty quantiles from the simulatedEigenValues in a PA (Parallel analysis) object
  # #
  # # Arg:
  # #  x: An object of class PA which is a list with data.frames observed, percentiles and simulated data.
  # #   observed: data.frame containing the observed eigenvalues
  # #   percentiles: data.frame containing the estimated percentiles of the eigenvalues distribution under independence
  # #   simulatedEigenValues: data.frame containing the simulated eigenvalues under independence
  # #  percentiles: vector of percentiles to report
  # #
  # # Ret:
  # #  ParallelList: An object of class PA with percentiles data.frame replaced for those from the percentiles argument
  
  ################################################################################
  # # Check object's class
  ################################################################################
  isPA <- Check.PA(x)
  if (!isPA) {
    stop("x must be of class PA")
  } 

  ################################################################################
  # # Obtain percentiles of simulated data
  ################################################################################
  estimatedPercentiles <- sapply(x$simulatedEigenValues, quantile, percentiles)
  
  nVariables <- nrow(x$observed)

  if (length(percentiles) == 1) {
    estimatedPercentiles <- data.frame(orderEigenValues = 1:nVariables,
                                       typeEigenValues  = 100 * percentiles,
                                       eigenValues      = estimatedPercentiles)
    rownames(estimatedPercentiles) <- 1:nVariables
  } else {
    estimatedPercentiles <- data.frame(t(estimatedPercentiles))
    pValues              <- grep("^X\\d+\\.$", names(estimatedPercentiles))
    names(estimatedPercentiles)[pValues] <- gsub("^X(\\d+)\\.$", "p.\\1", names(estimatedPercentiles)[pValues])

    estimatedPercentiles <- reshape(estimatedPercentiles, direction = "long",
                                    varying = seq(along = names(estimatedPercentiles)))
    estimatedPercentiles <- data.frame(orderEigenValues = estimatedPercentiles[, "id"],
                                       typeEigenValues  = estimatedPercentiles[, "time"],
                                       eigenValues      = estimatedPercentiles[, "p"])
  }

  ################################################################################
  # # Output
  ################################################################################
  parallelList <- list(observed = x$observed, percentiles = estimatedPercentiles,
                       simulatedEigenValues = x$simulatedEigenValues)

  class(parallelList) <- "PA"

  return(parallelList)
}
