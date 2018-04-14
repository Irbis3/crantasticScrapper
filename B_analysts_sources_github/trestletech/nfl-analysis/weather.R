library(httr)
library(RJSONIO)

#' @param wu_key Your Weather Underground API key.
getHistoricWeather <- function(wu_key, zip=NULL, lat=NULL, long=NULL, month=NULL, day=NULL, year=NULL, date=NULL, format="%Y-%m-%d"){
  if (missing(zip) && (missing(lat) || missing(long))){
    stop("You must specify either the zip code or both lat and long.")    
  }
  
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
  
  if (!missing(zip)){
    loc <- zip
  } else{
    loc <- paste(lat, long, sep=",")
  }
  
  weather <- GET(paste("http://api.wunderground.com/api/",
                       wu_key
                       ,"/history_",
                       year,
                       month,
                       day,
                       "/q/",
                       loc,
                       ".json", sep=""))
  
  json <- fromJSON(content(weather, as="text"))
  json <- json$history
  
  #check the date
  if (json$date["year"] != year ||
        json$date["mon"] != month ||
        json$date["mday"] != day){
    stop("Invalid date returned.")
  }
  
  
  #sometimes daily summary is empty, and no one seems to eager to debug, so we'll to it manually.
  #http://apicommunity.wunderground.com/weatherapi/topics/empty_daily_summary_value
  
  attr <- names(json$observations[[1]])
  attr <- attr[!attr %in% c("date", "utcdate", "metar")]
  
  data <- sapply(json$observations, "[", attr)    
  #get rid of this "-999" jibberish
  suppressWarnings(data[as.integer(data) == -999] <- NA)
  suppressWarnings(data[as.integer(data) == -9999] <- NA)
  
  #remove all "-999" rows
  data <- data[!apply(data, 1, function(x){all(is.na(x))}),]  
  
  #likely a better way to cast the numeric values to numbers, but we'll do this for now...
  suppressWarnings(numData <- {apply(data, 2, as.numeric)})

  dataLst <- list()  
  
  numData <- apply(numData, 1, mean, na.rm=TRUE)
  
  dataLst[rownames(data)[!is.nan(numData)]] <- 
    numData[!is.nan(numData)]
  
  #now need to identify the primary element in the non-numeric fields
  dataLst[rownames(data)[is.nan(numData)]] <- 
    apply(data[is.nan(numData),], 1, function(x){names(sort(table(as.character(x)), decreasing=TRUE))[1]})
    
  json <- dataLst
  
  json
}