# Functionality to read in the CRAN data and make sure it's all tickety-boo.

# Get data from Mirmom about CRAN mirror availability and updatedness. Necessary to
# avoid pointing people to an outdated place. Includes code from library(tools)
# in r-devel so we know it'll work on non-devel versions (since devel isn't standard
# on ubuntu and we probably don't want to be using the developer version - we want to
# be able to use the stable one).
update_mirmon_data <- function(){
  
  # Fields for the Mirmon files
  fields <- c("url",
              "age",
              "status_last_probe",
              "time_last_successful_probe",
              "probe_history",
              "state_history",
              "last_probe")
  
  # Simple timestamp converter
  posix_convert <- function(ts){
    suppressWarnings(as.POSIXct(as.numeric(as.character(ts)), origin = "1970-01-01"))
  }
  
  # State URLs
  state_files <- c("mirror.state", "mirror_release.state", "mirror_old_release.state")
  
  # Read the data in, generate a time delta, filter it to just the time delta and the URL
  # (since we need the URL for merging).
  mirror_states <- do.call(rbind, lapply(state_files, function(file){
    data <- utils::read.table(paste0("https://cran.r-project.org/mirmon/state/", file), header = FALSE,
                              col.names = fields, stringsAsFactors = FALSE)
    data$time_since_probe <- difftime(Sys.time(), posix_convert(data$age), units = "days")
    data <- data[,c("url","time_since_probe")]
    return(data)
  }))
  
  # Filter out those with >1.08
  failed_mirrors <- mirror_states$url[mirror_states$time_since_probe >= 1.08]
  good_mirrors <- unique(mirror_states$url[!mirror_states$url %in% failed_mirrors])
  return(good_mirrors)
}

# Read in and reformat the latest CRAN dataset. What we're interested
# in is the country code and whether it is HTTPS or not.
update_cran_data <- function(){
  
  # Get the CRAN mirror data
  original_data <- readr::read_csv("http://cran.r-project.org/CRAN_mirrors.csv")
  mirror_data <- data.frame(url = original_data$URL,
                            country = toupper(original_data$CountryCode),
                            is_https = grepl(x = original_data$Name, pattern = "[https]", fixed = TRUE),
                            stringsAsFactors = FALSE)
  
  # Get and incorporate the Mirmon data.
  # This lets us exclude mirrors more than 1.08 days (25-6 hours) out of date.
  mirror_data <<- mirror_data[mirror_data$url %in% update_mirmon_data(), ]
  
  # Set timestamp so we know not to check again, return invisibly.
  mirmon_ts <<- Sys.time()
  return(invisible())
}

# Updates the geolocation database we're using. This updates the first Tuesday of every month, so we're going
# to check once every 30 days for the initial version.
update_geo_data <- function(){
  
  file_temp_location <- paste0(local_geolocation_file,".gz")
  download.file(remote_geolocation_file,
                destfile = file_temp_location, quiet = TRUE)
  R.utils::gunzip(file_temp_location, overwrite = TRUE)
  geo_ts <<- Sys.Date()
  return(invisible())
}
