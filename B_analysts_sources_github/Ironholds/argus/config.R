# Dependencies
library(plumber) #devtools::install_github("trestletech/plumber")
library(rgeolocate) # on CRAN
library(readr) # on CRAN
library(R.utils)

# Variables
mirmon_ts <- Sys.time() - 14401 # The default should be <1 day so it's always updating mirmon on the first request.
geo_ts <- NULL # We'll have to set this as null, it's too thorny to just handle deterministically from the get-go.
local_geolocation_file <- "./data/city_data.mmdb"
remote_geolocation_file <- "http://geolite.maxmind.com/download/geoip/database/GeoLite2-Country.mmdb.gz"

dir.create("./data/", showWarnings = FALSE)
