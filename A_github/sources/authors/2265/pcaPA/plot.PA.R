plot.PA <- function (x, percentiles = NULL, main = NULL, xlab = NULL, ylab = NULL, groupLabel = NULL, colour = TRUE,
                     linetype = TRUE, observed = "Observed", percentile = "th percentile", position = "after", sep = "", ...) {
  # # Plots the scree plot for a PA object for the selected percentiles
  # #
  # # Arg:
  # #  PA: an object of class PA
  # #  percentiles: The percentiles that ought to be plotted, defaults to those in the object
  # #  main: Graph title instead of default
  # #  xlab: Label for x axis instead of default
  # #  ylab: Label for y axis instead of default
  # #  groupLabel: Legend box name instead of default
  # #  colour: Logical indicating whether to identify the observed eigenvalues and percentiles by colour
  # #  linetype: Logical indicating whether to identify the observed eigenvalues and percentiles by linetype
# #  observed: Label to the observed data, default is "observed"
  # #  percentile: Label for the percentiles
  # #  position: Position for the percentile label. "after" will position the
  # #  label after the percentile number. "before" will position the label
  # #  before the percentile number
  # #  sep: Character string to separate the label from the percentiles number
  # #
  # # Ret:
  # #  ggplotPA: ggplot object for plotting the scree plot

  orderEigenValues <- NULL
  typeEigenValues <- NULL
  eigenValues <- NULL


  ################################################################################
  # # Check object's class
  ################################################################################
  isPA <- Check.PA(x)
  if (!isPA) {
    stop("x must be of class PA")
  }

  if (!is.null(percentiles)) {
    x <- quantile.PA(x, percentiles = percentiles)
  }

  if (!colour & !linetype) {
    stop("The lines must be distinguished by colour or linetype")
  }

  # # Label Control
  nVariables <- nrow(x$observed)
  x$observed[,"typeEigenValues"] <- factor(rep(observed,nVariables))
  if (position == "after")
    x$percentiles[,"typeEigenValues"] <- paste(x$percentiles[,"typeEigenValues"], percentile, sep = sep)
  else x$percentiles[,"typeEigenValues"] <- paste(percentile, x$percentiles[,"typeEigenValues"], sep = sep)


    levels(x$observed[, "typeEigenValues"]) <- levels(x$percentiles[, "typeEigenValues"]) <-
      unique(c(levels(factor(x$observed[, "typeEigenValues"])), names(table(x$percentiles[, "typeEigenValues"]))))
    PA <- rbind(x$observed, x$percentiles)



    # # Create the plot
    ggplotPA <- ggplot(PA, aes(x = orderEigenValues, y = eigenValues))
    if (colour & linetype) {
      ggplotPA <- ggplotPA + geom_line(aes(colour = typeEigenValues, linetype = typeEigenValues))
    } else if (colour) {
      ggplotPA <- ggplotPA + geom_line(aes(colour = typeEigenValues))
    } else if (linetype) {
      ggplotPA <- ggplotPA + geom_line(aes(linetype = typeEigenValues))
    }



    # # Add labels
    if (!is.null(main)) {
      ggplotPA <- ggplotPA + labs(title = main)
  } else {
    ggplotPA <- ggplotPA + ggtitle("Parallel analysis scree-plot")
  }
  if (!is.null(xlab)) {
    ggplotPA <- ggplotPA + xlab(xlab)
  } else {
    ggplotPA <- ggplotPA + xlab("Eigenvalue order")
  }
  if (!is.null(ylab)) {
    ggplotPA <- ggplotPA + ylab(ylab)
  } else {
    ggplotPA <- ggplotPA + ylab("Eigenvalue")
  }
  if (!is.null(groupLabel)) {
    if (colour & linetype) {
      ggplotPA <- ggplotPA + labs(colour = groupLabel, linetype = groupLabel)
    } else if (colour) {
      ggplotPA <- ggplotPA + labs(colour = groupLabel)
    } else if (linetype) {
      ggplotPA <- ggplotPA + labs(linetype = groupLabel)
    }
  }

  return(ggplotPA)
}
