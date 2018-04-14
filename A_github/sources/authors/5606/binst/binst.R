#' Creates bins given breaks
#'
#' @param x X is a numeric vector which is to be discretized
#' @param breaks Breaks are the breaks for the vector X to be broken at. This excludes endpoints
#' @param method the approach to bin the variable, can either be cuts or hinge.
#' @return A vector same length as X is returned with the numeric discretization
#' @seealso\code{\link{create_breaks}}
#' @examples
#' create_bins(1:10, c(3, 5))
#' @export
create_bins <- function(x, breaks, method="cuts") {
  if (method=="hinge") {
    hingebins <- Reduce(cbind, lapply(breaks, function(bk) {
      cbind(pmax(0, x-bk), pmax(0, bk-x))
    }))
    colnames(hingebins) <- colnames(hingebins, do.NULL=FALSE)
    return(hingebins)
  } else if (method=="cuts") {
    return(cut(x, c(-Inf, Inf, breaks), labels = FALSE))
  } else {
    warning("Invalid method found, please choose either \"cuts\" or \"hinge\". Defaulting to \"cuts\".")
    return(cut(x, c(-Inf, Inf, breaks), labels = FALSE))
  }
}

#' A convenience functon for creating breaks with various methods.
#'
#' @param x X is a numeric vector to be discretized
#' @param y Y is the response vector used for calculating metrics for discretization
#' @param method Method is the type of discretization approach used. Possible methods are: "dt", "entropy", "kmeans", "jenks"
#' @param control Control is used for optional parameters for the method. It is a list of optional parameters for the function
#' @param ... instead of passing a list into control, arguments can be parsed as is.
#' @return A vector containing the breaks
#' @seealso \code{\link{get_control}}, \code{\link{create_bins}}
#' @importFrom stats complete.cases
#' @examples
#' kmeans_breaks <- create_breaks(1:10)
#' create_bins(1:10, kmeans_breaks)
#'
#' # passing the k means parameter "centers" = 4
#' kmeans_breaks <- create_breaks(1:10, list(centers=4))
#' create_bins(1:10, kmeans_breaks)
#'
#' entropy_breaks <- create_breaks(1:10, rep(c(1,2), each = 5), method="entropy")
#' create_bins(1:10, entropy_breaks)
#'
#' dt_breaks <- create_breaks(iris$Sepal.Length, iris$Species, method="dt")
#' create_bins(iris$Sepal.Length, dt_breaks)
#' @export
create_breaks <- function(x, y=NULL, method="kmeans", control=NULL, ...) {
  if (!is.null(y)){
    complete <- complete.cases(x) & complete.cases(y)
    if (!all(complete)){
      warning("x or y vector contains NA. These observations will be automatically removed.")
      x <- x[complete]
      y <- y[complete]
    }
  } else {
    complete <- complete.cases(x)
    if (!all(complete)) {
      warning("x vector contains NA. These observations will be automatically removed.")
      x <- x[complete]
    }
  }

  control <- c(control, list(...))
  method <- tolower(method)
  if (method == "kmeans") {
    return(create_kmeansbreaks(x, control=control))
  }
  if (method %in% c("mdlp", "entropy")) {
    return(create_mdlpbreaks(x, y))
  }
  if (method %in% c("decisiontrees", "dt", "ctrees")) {
    return(create_dtbreaks(x, y, control=control))
  }
  if (method == "jenks") {
    return(create_jenksbreaks(x, control=control))
  }
  if (method %in% c("earth", "mars")) {
    return(create_earthbreaks(x, y, control=control))
  }
}

#' gets the default parameters for each method.
#'
#' @param method Method is the type of discretization approach used
#' @param control Control are the controls for the algorithm
#' @export
#' @return List of default parameters
get_control <- function(method="kmeans", control=NULL) {
  if (method=="kmeans"){
    if (class(control)=="list" & length(control) == 0){
      return(list(centers=3))
    } else if ("centers" %in% names(control)){
      return(control)
    } else {
      return(c(list(centers=3), control))
    }
  }
  if (method=="jenks"){
    if (class(control)=="list" & length(control) == 0){
      return(list(k=3))
    } else if ("k" %in% names(control)){
      return(control)
    } else {
      return(c(list(k=3), control))
    }
  }
  if (method=="dt"){
    if (class(control)=="list" & length(control) == 0){
      return(list())
    } else {
      return(control)
    }
  }
  if (method=="earth") {
    if (class(control)=="list" & length(control) == 0){
      return(list())
    } else {
      return(control)
    }
  }
}

#' Create kmeans breaks.
#'
#' @param x X is a numeric vector to be discretized
#' @param control Control is used for optional parameters for the method
#' @importFrom stats kmeans aggregate
#' @return A vector containing the breaks
#' @seealso\code{\link{create_breaks}}
#' @examples
#' kmeans_breaks <- create_breaks(1:10)
#' create_bins(1:10, kmeans_breaks)
#' @export
create_kmeansbreaks <- function(x, control=NULL) {
  model <- do.call(stats::kmeans, c(list(x=x), get_control("kmeans", control)))
  n_center <- get_control("kmeans", control)$centers
  df <- data.frame(x=x, y=model$cluster)
  minx <- aggregate(x~y, data =df, min)$x
  maxx <- aggregate(x~y, data =df, max)$x
  minmax <- unique(c(minx, maxx))
  minmax <- minmax[minmax != min(minmax) & minmax != max(minmax)]
  minmax <- sort(minmax)
  return(sort(as.numeric(kmeans(minmax, centers=n_center-1)$centers)))
}

#' Create Jenks breaks
#'
#' @param x X is a numeric vector to be discretized
#' @param control Control is used for optional parameters for the method
#' @return A vector containing the breaks
#' @seealso\code{\link{create_breaks}}
#' @examples
#' jenks_breaks <- create_breaks(1:10, method="jenks")
#' create_bins(1:10, jenks_breaks)
#' @export
create_jenksbreaks <- function(x, control=NULL) {
  if (! requireNamespace("BAMMtools", quietly = TRUE)) { # nocov start
    stop("Please install BAMMtools: install.packages('BAMMtools')")
  } # nocov end
  k <- get_control("jenks", control)$k
  breaks <- do.call(BAMMtools::getJenksBreaks, c(list(var=x), k+2))

  if (length(breaks) <= k){ # nocov start
    return(breaks)
  } else {
    return(breaks[breaks != min(breaks) & breaks != max(breaks)])
  } # nocov end
}


#' Create breaks using mdlp
#'
#' @param x X is a numeric vector to be discretized
#' @param y Y is the response vector used for calculating metrics for discretization
#' @return A vector containing the breaks
#' @seealso\code{\link{create_breaks}}
#' @examples
#' entropy_breaks <- create_breaks(1:10, rep(c(1,2), each = 5), method="entropy")
#' create_bins(1:10, entropy_breaks)
#' @export
create_mdlpbreaks <- function(x, y) {
  if (! requireNamespace("discretization", quietly = TRUE)) { # nocov start
    stop("Please install discretization: install.packages('discretization')")
  } # nocov end
  return(discretization::cutPoints(x,y))
}

#' Create breaks using earth (i.e. MARS)
#'
#' @param x X is a numeric vector to be discretized
#' @param y Y is the response vector used for calculating metrics for discretization
#' @param control Control is used for optional parameters for the method
#' @return A vector containing the breaks
#' @seealso\code{\link{create_breaks}}
#' @examples
#' earth_breaks <- create_breaks(x=iris$Sepal.Length, y=iris$Sepal.Width, method="earth")
#' create_bins(iris$Sepal.Length, earth_breaks)
#' @export
create_earthbreaks <- function(x, y, control=NULL) {
  if (! requireNamespace("earth", quietly = TRUE)) { # nocov start
    stop("Please install earth: install.packages('earth')")
  } # nocov end
  earth_model <- do.call(earth::earth, c(list(x=x, y=y), get_control("earth", control)))$cut
  return(unique(earth_model[!(rownames(earth_model) %in% c("(Intercept)"))]))
}


#' Create breaks using decision trees (recursive partitioning)
#'
#' @param x X is a numeric vector to be discretized
#' @param y Y is the response vector used for calculating metrics for discretization
#' @param control Control is used for optional parameters for the method
#' @return A vector containing the breaks
#' @seealso\code{\link{create_breaks}}
#' @examples
#' dt_breaks <- create_breaks(iris$Sepal.Length, iris$Species, method="dt")
#' create_bins(iris$Sepal.Length, dt_breaks)
#' @export
create_dtbreaks <- function(x, y, control=NULL) { # nocov start
  df <- data.frame(x=x, y=y)
  build_tree <-do.call(rpart::rpart, c(list(formula=y~x, data=df), get_control("dt", control)))
  breaks <- as.numeric(unique(build_tree$splits[, "index"]))
  return(breaks)
}
