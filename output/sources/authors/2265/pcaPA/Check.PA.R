Check.PA <- function (PA) {
  # # Checks if PA is a PA (Parallel analysis) object.
  # #
  # # Arg:
  # #  PA: An object of class PA which is a list with data.frames observed, percentiles and simulated data.
  # #   observed: data.frame containing the observed eigenvalues
  # #   percentiles: data.frame containing the estimated percentiles of the eigenvalues distribution under independence
  # #   simulatedEigenValues: data.frame containing the simulated eigenvalues under independence
  # #  percentiles: vector of percentiles to report
  # #
  # # Ret:
  # #  PA: An object of class PA with percentiles data.frame replaced for those from the percentiles argument
  
  isPA <- class(PA) == "PA"
  components <- c("observed", "percentiles", "simulatedEigenValues")

  isComponents <- all(names(PA) %in% components) & all(components %in% names(PA))

  isPA <- isPA && isComponents

  if (isPA) {
    namesEigen    <- c("orderEigenValues", "typeEigenValues", "eigenValues")
    isObserved    <- is.data.frame(PA[["observed"]]) && names(PA[["observed"]]) == namesEigen
    isPercentiles <- is.data.frame(PA[["percentiles"]]) && names(PA[["percentiles"]]) == namesEigen
    isSimulated   <- all(sapply(PA[["simulatedEigenValues"]], is.numeric))

    isPA <- isPA && isObserved && isPercentiles && isSimulated
  } 
  return(isPA)
}
