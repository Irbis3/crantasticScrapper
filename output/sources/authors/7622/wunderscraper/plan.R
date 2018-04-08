#' Plan a schedule for executing a task
#'
#' Sets a schedule using strptime
#'
#' @param scheduler A \code{\link{scheduler}} object.
#' @param ... Arguments passed to \code{\link{seq.POSIXt}}.
#' @return The schedule
#' @seealso \code{\link{strptime}}, \code{\link{seq.POSIXt}}
#' @examples
#' plan(scheduler(), '1 hour') # sample every hour
#' plan(scheduler(), '30 min') # sample every 30 minutes
#' @export
plan <- function(scheduler, ...) UseMethod('plan')
plan.default <- function(x, ...) warning(paste0('plan cannot handle class ', class(x)))

#' @describeIn plan convenience wrapper around seq.POSIXt.
#' @export
plan.scheduler <- function(scheduler, ...) {
    scheduler $schedule <- seq(strptime(0, '%H'), strptime(23, '%H'), ...)
    scheduler $schedule
}
