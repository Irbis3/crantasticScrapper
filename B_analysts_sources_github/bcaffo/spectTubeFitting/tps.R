#############################################################
# Nov 2007
# Brian Caffo
#
# some spline utility functions
#############################################################

tps <- function(x, df = NULL, knots = "None"){
  if (is.null(df) & all(knots == "None")) stop("Error tps, df and knots missing")
  if (is.null(df)){
    df <- 3 + knots
  }
  if (df == 1) w = x
  else if (df == 2) w = cbind(x, x ^ 2)
  else if (df == 3) w = cbind(x, x ^ 2, x ^ 3)
  else if (df >= 4){
    if (all(knots == "None")){
      temp <- df - 3
      qtiles <- seq(0, 1, length = temp + 2)[-c(1, temp + 2)]
      knots <- quantile(x, qtiles)
    }
    w = cbind(x, x ^ 2, x ^ 3, sapply(knots, function(k) ((x - k > 0) * (x - k)) ^ 3))
  }
  class(w) <- "tps"
  attr(w, "df") <- df
  attr(w, "knots") <- knots
  return(w)
}

##dones this way so that we can have a predict function
predict.tps <- function(basis, newX){
  return(tps(newX, df = attributes(basis)$df, knots = attributes(basis)$knots))
}

deriv <- function(x, df, knots){
  if (df == 1) d = 1
  else if (df == 2) cbind(1, 2 * x)
  else if (df == 3) cbind(1, 2 * x, 3 * x ^ 2)
  else if (df > 4){
    d = cbind(1, 2 * x, 3 * x ^ 2, sapply(knots, function(k) (3 * (x - k > 0) * (x - k) ^ 2)))
  }
  return(d)
}
