#' Schedules wunderscraper
#'
#' Schedule sampling and ensure wunderscraper remains within API usage limits.
#'
#' Scheduler is a constructor function that returns a scheduler object for use
#' in a wunderscrape process.
#'
#' Scheduler has methods for managing the schedule: \code{\link{plan}}, and
#' \code{\link{sync}}.
#' @param plan API usage plan.  Possible values are "developer" (500 calls a day
#' 10 calls a minute, "drizzle" (5000 calls a day 100 calls a minute) "shower"
#' (100000 calls a day 1000 a minute), or "custom" (see parameters \code{day}
#' and \code{minute}).
#' @param day Custom daily API usage limit.
#' @param minute Custom minute API usage limit.
#' @return Returns a scheduler object.
#' @seealso \code{\link{plan.scheduler}}, \code{\link{sync.scheduler}}
#' @examples
#' scheduler(plan='drizzle')
#' @export
scheduler <- function(plan='developer', day=NA, minute=NA) {
    e <- structure(new.env(), class='scheduler') # use environment for reference semantics
    e $date=format(Sys.Date(), tz='America/New_York')
    e $n <- 0
    e $plan <- plan
    e $limits <- list(developer=c(500, 10), drizzle=c(5000, 100), shower=c(1e5, 1e3),
                      custom=c(day, minute))
    e $schedule <- seq(strptime(0, '%H'), strptime(23, '%H'), '1 hour') # default schedule
    e
}

.schedule <- function(scheduler) UseMethod('.schedule')
.scehdule.default <- function(x) warning(paste0('.schedule cannot handle class ', class(x)))

.schedule.scheduler <- function(scheduler) {
    ## schedule and ensure api calls remain within minute and daily limits
    limits <- with(scheduler, limits[[plan]])
    repeat{
        if(scheduler $schedule[1]<Sys.time()) break # wait till start time
        Sys.sleep(Sys.getenv('WUNDERSCRAPER_SLEEP'))
    }
    repeat{
        d <- format(Sys.Date(), tz='America/New_York')
        if(scheduler $date<d) {
            scheduler $n <- 0
            scheduler $date <- d
            sync(scheduler)
        }
        if(scheduler $n<limits[1]) break # daily limits
        Sys.sleep(Sys.getenv('WUNDERSCRAPER_SLEEP'))
    }
    Sys.sleep(61/limits[2]) # minute limits
    scheduler $n <- scheduler $n + 1
}
