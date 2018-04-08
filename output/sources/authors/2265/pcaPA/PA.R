################################################################################
# # Generate PA object
################################################################################
PA <- function (dataMatrix, percentiles = 0.99, nReplicates = 200, type = "continuous", use = "complete.obs", algorithm = "polycor") {
  # # Creates an objecto of class PA using one of the PA functions
  # #
  # # Arg:
  # #  dataMatrix: matrix or data.frame of continuous numeric variables
  # #  percentiles: vector of percentiles to report
  # #  nReplicates: number of simulations to produce for estimating the eigenvalues distribution under independence
  # #
  # # Ret:
  # #  PA: an object of class PA which is a list with data.frames observed, percentiles and simulated data.
  # #   observed: data.frame containing the observed eigenvalues
  # #   percentiles: data.frame containing the estimated percentiles of the eigenvalues distribution under independence
  # #   simulatedEigenValues: data.frame containing the simulated eigenvalues under independence
  
  if( type != "binary" && type != "ordered" && type != "continuous" && type != "mixed") stop ("type must be binary, ordered, continuous or mixed") 

  ObtainPA <- switch(type,
                     continuous = CalculatePAContinuous,
                     ordered    = CalculatePAOrdered,
                     binary     = CalculatePABinary,
                     mixed      = CalculatePAMixed)

  PA <- ObtainPA(dataMatrix = dataMatrix, percentiles = percentiles, nReplicates = nReplicates, use = use, algorithm = algorithm)

  class(PA) <- "PA"
  return(PA)
}
