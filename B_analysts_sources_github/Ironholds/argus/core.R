source("config.R")
source("update_data.R")
source("find_mirror.R")

# The main functionality; we shouldn't need anything other than this,
# honestly
#' @get /mirror
main <- function(https = 0, req, res){
  
  # Check we have the latest version of the mirnon file and CRAN data,
  # If not we need to read those in.
  if((mirmon_ts + 14400) < Sys.time()){
    update_cran_data()
  }

  # Check we have the latest version of the underlying geolocation data.
  # If not we need to read in the new one
  if(is.null(geo_ts) || geo_ts + 30 < Sys.Date()){
    update_geo_data()
  }

  # Check the user agent; we don't care if it's not R.
  if(!grepl(x = req$HTTP_USER_AGENT, pattern = "^R \\(")){
    res$status <- 403
    res$body <- "This service only responds to requests from R instances"
    return(res)
  }

  # Make sure the https parameter is actually set. If it isn't,
  # return a bad request
  https <- suppressWarnings({as.integer(https)})
  if(!https %in% c(0,1)){
    res$status <- 400
    res$body <- "The 'https' parameter must be 0 or 1"
    return(res)
  }
  
  # If we're still here, it's a good UA and http(s) was set, let's geolocate and
  # identify the appropriate project.
  res$status <- 200
  res$body <- paste0(find_mirror(req$REMOTE_ADDR, https), "\n")
  return(res)
}
