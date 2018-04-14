library(httr)
library(RJSONIO)

#' @param wu_key Your Weather Underground API key.
getHistoricWeather <- function(wu_key=options("wu_key")[[1]], zip, month=NULL, day=NULL, year=NULL, date=NULL, format="%Y-%m-%d"){
  if (!missing(date)){
      date <- as.Date(date)
      year <- year(date)
      month <- month(date)
      day <- day(date)
  } 
  if (nchar(day) == 1){
    day <- paste("0", day, sep="")
  }
  if (nchar(month) == 1){
    month <- paste("0", month, sep="")
  }
  if (nchar(year) == 2){
    year <- paste("20", year, sep="")
  }
  
  weather <- GET(paste("http://api.wunderground.com/api/",
                       wu_key
                       ,"/history_",
                       year,
                       month,
                       day,
                       "/q/",
                       zip,
                       ".json", sep=""))
  
  json <- fromJSON(content(weather, as="text"))
  json <- json$history
  
  #check the date
  if (json$date["year"] != year ||
    json$date["mon"] != month ||
    json$date["mday"] != day){
    stop("Invalid date returned.")
  }
    
  json <- json$dailysummary[[1]]
  
  json  
}