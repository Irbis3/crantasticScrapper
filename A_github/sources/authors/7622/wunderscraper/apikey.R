#' Set API key.
#'
#' Sets the API key to the value of the argument.
#'
#' If \code{f} is not missing, then reads the API key from a file, else sets
#' key to the \code{key} parameter.  Does not check if key is valid.  Please
#' visit <https://www.wunderground.com/weather/api/d/pricing.html> to sign up
#' for a free API key.
#'
#' @param key A valid Wunderground API key
#' @param f A string indicating a text file containing only an API key
#' @return The API key
#' @examples
#' setApiKey('q1w2e3r4t5y6u7i8') # not a valid key
#' @export
setApiKey <- function(key, f=NULL) {
    if(!is.null(f)) key <- trimws(readChar(f, file.info(f)$size))
    Sys.setenv(WUNDERSCRAPER_KEY=key)
    key
}

.getApiKey <- function() {
    if(identical(key <- Sys.getenv('WUNDERSCRAPER_KEY'), '')) {
        stop(strwrap('use setApiKey to set key.  Please visit
                      <https://www.wunderground.com/weather/api/d/pricing.html>
                      to sign up for an API key.'))
    }
    key
}
