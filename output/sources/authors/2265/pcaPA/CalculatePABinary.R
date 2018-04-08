################################################################################
# # Function for parallel analysis of correlation matrices of Dichotomous data
################################################################################
CalculatePABinary <- function (dataMatrix, percentiles = 0.99, nReplicates = 200, use = "complete.obs", algorithm = "polycor") {
  # # Obtains a parallel analysis for dichotomous data
  # #
  # # Arg:
  # #  dataMatrix: matrix or data.frame of binary or dichotomous variables
  # #  percentiles: vector of percentiles to report
  # #  nReplicates: number of simulations to produce for estimating the eigenvalues distribution under independence
  # #  use: Missing value handling method: If "complete.obs", remove observations with any missing data; if "pairwise.complete.obs", compute each correlation using all observations with valid data for that pair of variables.
  # #  algorithm: string specifying the correlation estimation algorithm. Polychoric correlation estimation method: "polycor" for estimates using the polycor package, "polychoric" for estimates using the C++ function polychoric.
  # #
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

  isBinaryData <- all(dataMatrix == 0 | dataMatrix == 1, na.rm = TRUE)
  if (!isBinaryData) {
    stop("All variables in dataMatrix must be binary values")
  }

  isOrderedData <- all(sapply(dataMatrix, is.ordered))
  if (!isOrderedData) {
    dataMatrix[, ] <- lapply(dataMatrix, as.ordered)
  }

  if (!(algorithm %in% c("polycor", "polychoric"))) {
    stop("Unknown algorithm")
  }

  ################################################################################
  # # Data information
  ################################################################################
  nObservations  <- nrow(dataMatrix)
  nVariables     <- ncol(dataMatrix)
  if (algorithm == "polycor") {
    datCorrelation <- hetcor(dataMatrix, pd = TRUE, use = use) # Polychoric correlations with matrix transformed to the nearest
                                                  # positive definite matrix
    datEigenValues <- eigen(datCorrelation)$values


  }
  if (algorithm == "polychoric") {
    isBinary = TRUE
    if (use == "complete.obs") {
      dataMatrix <- na.omit(dataMatrix)
    }

    dataMatrixInt <- as.matrix(as.data.frame.list(lapply(dataMatrix,
                                                           function(x)
                                                             as.integer(as.integer(x)-1))))

    result <- .Call(Cpolychoric, dataMatrixInt, isBinary,
                    nReplicates, sfsmisc::nearcor, new.env())
    datEigenValues <-sort(result[[1]], decreasing = TRUE)
  }

  observed <- data.frame(orderEigenValues   = 1:nVariables,
                           typeEigenValues  = "Observed",
                           eigenValues      = datEigenValues,
                           stringsAsFactors = TRUE)



  ################################################################################
  # # Simulate correlation matrices under independence
  ################################################################################

  if(algorithm == "polycor") {
    simulatedEigenValues <- matrix(nrow = nReplicates, ncol = nVariables)
    rownames(simulatedEigenValues) <- paste("iter", 1:nReplicates, sep = "")
    colnames(simulatedEigenValues) <- 1:nVariables

    marginalProp <- colMeans(apply(dataMatrix, 2, as.numeric))

    simulatedData <- matrix(nrow = nObservations, ncol = nVariables)
    simulatedData <- data.frame(simulatedData)
    for (ii in 1:nReplicates) {
      for (jj in 1:nVariables) {
        simulatedData[, jj]  <- ordered(rbinom(nObservations, size = 1, prob = marginalProp[jj]))
      }
      simulatedEigenValues[ii, ] <- eigen(hetcor(simulatedData, pd = TRUE, use = use))$values
    }
    simulatedEigenValues <- data.frame(simulatedEigenValues)
  }
  if (algorithm == "polychoric") {
    simulatedEigenValues <- matrix(result[[2]],ncol=nReplicates)
    simulatedEigenValues <- as.matrix(as.data.frame.list(tapply(simulatedEigenValues,
                                                                col(simulatedEigenValues),
                                                                sort,
                                                                decreasing = TRUE)))
    simulatedEigenValues           <- data.frame(t(simulatedEigenValues))
    rownames(simulatedEigenValues) <- paste("iter", 1:nReplicates, sep = "")
    colnames(simulatedEigenValues) <- 1:nVariables
  }


  ################################################################################
  # # Obtain percentiles of simulated data
  ################################################################################
  estimatedPercentiles <- sapply(simulatedEigenValues, quantile, percentiles)

  if (length(percentiles) == 1) {
    estimatedPercentiles <- data.frame(orderEigenValues = 1:nVariables,
                                       typeEigenValues  = 100 * percentiles,
                                       eigenValues      = estimatedPercentiles)
    rownames(estimatedPercentiles) <- 1:nVariables
  } else {
    estimatedPercentiles <- data.frame(t(estimatedPercentiles))
    pValues <- grep("^X\\d+\\.$", names(estimatedPercentiles))
    names(estimatedPercentiles)[pValues] <- gsub("^X(\\d+)\\.$", "p.\\1",
                                                 names(estimatedPercentiles)
                                                 [pValues])
    estimatedPercentiles <- reshape(estimatedPercentiles, direction = "long",
                                    varying = seq(along =
                                                    names(estimatedPercentiles)))
    estimatedPercentiles <- data.frame(orderEigenValues =
                                         estimatedPercentiles[, "id"],
                                       typeEigenValues =
                                         estimatedPercentiles[, "time"],
                                       eigenValues =
                                         estimatedPercentiles[, "p"])
  }

  ################################################################################
  # # Output
  ################################################################################
  parallelList <- list(observed = observed, percentiles = estimatedPercentiles,
                       simulatedEigenValues = simulatedEigenValues)

  return(parallelList)
}
