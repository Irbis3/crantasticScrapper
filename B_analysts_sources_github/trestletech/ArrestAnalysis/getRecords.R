library(lubridate)
library(httr)

#' Itemize all days in the specified format between the start date and the end date.
itemizeDates <- function(startDate, endDate, 
                         format="%Y-%m-%d") {
  out <- seq(as.Date(startDate, format=format), 
             as.Date(endDate, format=format), by="days")  
  format(out, format)
}

#' 
#' @param startDate The date to start from in the format of YYYY-MM-DD
#' @param endDate The date to end on in the format of YYYY-MM-DD
getWeather <- function(wu_key=options("wu_key")[[1]], zip, startDate, endDate, outputFile="weather.Rds", sleepInterval=7, format="%Y-%m-%d"){
  if (missing(wu_key) || is.null(wu_key)){
    stop("No Weather Underground API key provided")
  }
  
  allDays <- itemizeDates(startDate, endDate, format)
  
  pb <- txtProgressBar(min=0, max=length(allDays), initial=0, style=3)
  weather <- list(zip=zip)  
  counter <- 0
  for (d in allDays){    
    Sys.sleep(sleepInterval)
    tryCatch(weather[[d]] <- getHistoricWeather(wu_key, zip=zip, date=d), error=function(e){warning("Error downloading weather ", d)})
    saveRDS(weather, file=outputFile)
    
    counter <- counter+1
    setTxtProgressBar(pb, counter)
  }
  close(pb)
  weather
}