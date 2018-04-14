print.PA <- function (x, digits = max(3, getOption("digits") - 3), observed = "Observed", percentile = "th percentile", position = "after", sep = "", ...) {
  # # Prints the observed eigenvalues and the percentiles as a matrix
  # #
  # # Arg:
  # #  PA: an object of class PA
  # # digits: number of digits to print
  # #  observed: Label to the observed data, default is "observed" 
  # #  percentile: Label for the percentiles
  # #  position: Position for the percentile label. "after" will position the
  # #  label after the percentile number. "before" will position the label
  # #  before the percentile number
  # #  sep: Character string to separate the label from the percentiles number
 
  # #
  # # Ret:
  # #  NULL
  
  ################################################################################
  # # Check object's class
  ################################################################################
  isPA <- Check.PA(x)
  if (!isPA) {
    stop("x must be of class PA")
  } 
  
  levels(x$observed[, "typeEigenValues"]) <- levels(x$percentiles[, "typeEigenValues"]) <- 
    unique(c(levels(x$observed[, "typeEigenValues"]), names(table(x$percentiles[, "typeEigenValues"]))))
  PA <- rbind(x$observed, x$percentiles)

  wideEigenValues <- reshape(PA, direction = "wide", timevar = "typeEigenValues", idvar = "orderEigenValues")

  rownames(wideEigenValues) <- wideEigenValues[, "orderEigenValues"]
  colnames(wideEigenValues) <- gsub("eigenValues.", "", colnames(wideEigenValues))
  colnames(wideEigenValues) <- gsub("^(\\d+)", "p\\1", colnames(wideEigenValues))
  wideEigenValues           <- as.matrix(wideEigenValues[, -1])

  # # Label Control

  if (position == "after")
    dimnames(wideEigenValues)[[2]] <- c(observed, paste(unique(x$percentiles[,"typeEigenValues"]), percentile, sep = sep))
  else dimnames(wideEigenValues)[[2]] <- c(observed, paste(percentile, unique(x$percentiles[,"typeEigenValues"]), sep = sep))

  # # Print Section

  print.default(format(wideEigenValues, digits = digits), print.gap = 2, quote = FALSE)
  
  invisible(NULL)
}
