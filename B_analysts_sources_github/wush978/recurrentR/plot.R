#'@title Plot the Rate Function and Confidence Interval Estimated by recurrent-data
#'@param obj recurrent-data object.
#'@param methods character value. One of \code{c("none", "bootstrap", "asymptotic")}
#'@param B integer value. The size of bootstrap sample
#'@param confidence numeric value. The confidence of confidence interval.
#'@export
#'@examples
#'\dontrun{
#'library(survival)
#'data(MMC)
#'obj <- create_recurrent_data.data.frame(
#'  MMC, id = "id", time = "time", time_type = "relatively",
#'  indicator = "event", indicator_value = list("recurrent" = 1, "censor" = 0),
#'  covariate = "group"
#'  )
#'plot.rate.wang2001(obj)
#'plot.rate.wang2001(obj, "bootstrap")
#'plot.rate.wang2001(obj, "asymptotic")
#'}
plot.rate.wang2001 <- function(obj, 
                          methods = c("none", "bootstrap", "asymptotic"), 
                          B = 100, confidence = 0.95, ...) {
  methods <- methods[1]
  wang_2001 <- Wang2001(obj, methods)
  Lambda_0.hat <- wang_2001$Lambda_0.hat
  curve(Lambda_0.hat, 0, obj@T_0)
  if (methods == "none") {
    return(invisible(NULL))
  }
  Lambda_0.hat.upper <- function(t) {
    Lambda_0.hat(t) + qnorm((1 - confidence) / 2, lower.tail=FALSE) * sqrt(wang_2001$Lambda_0.hat.var(t))
  }
  Lambda_0.hat.lower <- function(t) {
    Lambda_0.hat(t) - qnorm((1 - confidence) / 2, lower.tail=FALSE) * sqrt(wang_2001$Lambda_0.hat.var(t))
  }
  curve(Lambda_0.hat.upper, add = TRUE, col = 2, lty = 2)
  curve(Lambda_0.hat.lower, add = TRUE, col = 2, lty = 2)
  return(invisible(NULL))
}