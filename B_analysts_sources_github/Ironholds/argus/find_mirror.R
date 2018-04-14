# The core functionality to find an appropriate mirror.
find_mirror <- function(ip_address, https){
  
  # Geolocate, and if we don't find anything, return easy-peasy.
  country <- rgeolocate::maxmind(ip_address, local_geolocation_file, "country_code")$country_code[1]
  if(country == "Unknown"){
    return("Unknown")
  }
  
  # Otherwise there /is/ something, in which case let's see what mirror data is appropriate.
  # If none, return unknown.
  local_mirrors <- mirror_data[mirror_data$country == country,]
  if(https){
    local_mirrors <- local_mirrors[local_mirrors$is_https == TRUE,]
  }

  if(!nrow(local_mirrors)){
    return("Unknown")
  }

  # If we're still here there are mirrors in local_mirrors that meet the user's needs. In the long term
  # we'll want to have mirnon detection inbuilt but for the time being let's just randomly sample.
  return(sample(local_mirrors$url, 1))
}
