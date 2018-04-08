################################################################################
# # Function for parallel analysis of correlation matrices of a mixed type of variables
################################################################################
CalculatePAMixed <- function (dataMatrix, percentiles = 0.99, nReplicates = 200, use = "complete.obs", algorithm = "polycor") {
  # # Obtains a parallel analysis for numeric and ordered mixed data
  # #
  # # Arg:
  # #  dataMatrix: matrix or data.frame of ordered variables
  # #  percentiles: vector of percentiles to report
  # #  nReplicates: number of simulations to produce for estimating the eigenvalues distribution under independence
  # #  algorithm: string specifying the correlation estimation algorithm. Should be "polycor" for mixed type data

  # #
  # # Ret:
  # #  parallelList: a list with data.frames observed, percentiles and simulated data.
  # #   observed: data.frame containing the observed eigenvalues
  # #   percentiles: data.frame containing the estimated percentiles of the eigenvalues distribution under independence
  # #   simulatedEigenValues: data.frame containing the simulated eigenvalues under independence



  ################################################################################
  # # Data verification
  ################################################################################
  if (!is.matrix(dataMatrix) & !is.data.frame(dataMatrix)) {
    stop("dataMatrix must be a matrix or a data.frame")
  }
  if (!is.data.frame(dataMatrix)) {
    dataMatrix <- data.frame(dataMatrix)
  }

  isNominalData <- sapply(dataMatrix, is.numeric) || sapply(dataMatrix, is.ordered)
  if (!isNominalData) {
    stop("All variables in dataMatrix must be either numeric or ordered factors")
  }

  if (algorithm != "polycor") {
    stop("For mixed data algorithm must be 'polycor'")
  }

  ################################################################################
  # # Data information
  ################################################################################
  nObservations  <- nrow(dataMatrix)
  nVariables     <- ncol(dataMatrix)
  datCorrelation <- hetcor(dataMatrix, pd = TRUE, use = use) # Heterogeneous correlations (Pearson, Polychoric or Polyserial)
                                                  # with matrix transformed to the nearest positive definite matrix
  datEigenValues <- eigen(datCorrelation)$values

  observed <- data.frame(orderEigenValues = 1:nVariables,
                         typeEigenValues  = "Observed",
                         eigenValues      = datEigenValues,
                         stringsAsFactors = TRUE)

  ################################################################################
  # # Simulate correlation matrices under independence
  ################################################################################
  simulatedEigenValues           <- matrix(nrow = nReplicates, ncol = nVariables)
  rownames(simulatedEigenValues) <- paste("iter", 1:nReplicates, sep = "")
  colnames(simulatedEigenValues) <- 1:nVariables

  simulatedData <- matrix(nrow = nObservations, ncol = nVariables)
  simulatedData <- data.frame(simulatedData)
  for (ii in 1:nReplicates) {
    for (jj in 1:nVariables) {
      isNumeric <- is.numeric(dataMatrix[, jj])
      isOrdered <- is.ordered(dataMatrix[, jj])

      if (isNumeric) {
        simVector <- rnorm(n = nObservations)
      } else if (isOrdered) {
        probVariable         <- prop.table(table(dataMatrix[, jj]))
        simulatedMultinomial <- rmultinom(n = nObservations, size = 1, prob = probVariable)
        simVector            <- ordered(row(simulatedMultinomial)[simulatedMultinomial == 1])
      } else {
        stop("All variables in dataMatrix must be either numeric or ordered factors")
      }
      simulatedData[, jj]  <- simVector
    }

    simulatedEigenValues[ii, ] <- eigen(hetcor(simulatedData, pd = TRUE, use = use))$values
  }
  simulatedEigenValues <- data.frame(simulatedEigenValues)

  ################################################################################
  # # Obtain percentiles of simulated data
  ################################################################################
  estimatedPercentiles <- sapply(simulatedEigenValues, quantile, percentiles)

  if (length(percentiles) == 1) {
    estimatedPercentiles <- data.frame(orderEigenValues = 1:nVariables,
                                       typeEigenValues = 100 * percentiles,
                                       eigenValues = estimatedPercentiles)
    rownames(estimatedPercentiles) <- 1:nVariables
  } else {
    estimatedPercentiles <- data.frame(t(estimatedPercentiles))
    pValues <- grep("^X\\d+\\.$", names(estimatedPercentiles))
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
  parallelList <- list(observed = observed, percentiles = estimatedPercentiles,
                       simulatedEigenValues = simulatedEigenValues)

  return(parallelList)
}
