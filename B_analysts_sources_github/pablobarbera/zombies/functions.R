#==============================================================================
# FUNCTIONS USED IN THE ZOMBIE OUTBREAK PREDICTOR
#==============================================================================

#==============================================================================
# EXTRACT TWEETS USING STREAMING API
#==============================================================================

getStream <- function(string, filename, time=10800, user, password){
        require(RCurl)
        # user and password
        userpwd <- paste(user, password, sep=":")
        # Strings to search
        string_nospace <- gsub(" ", "_", string)
        if (length(string)>1){
                string <- paste(string, collapse=",")
        }
        track <- paste("track=", string, sep="")
        #### Function: redirects output to a file
        WRITE_TO_FILE <- function(x) {
                if (nchar(x) >0 ) {
                        write.table(x, file=filename, append=T, 
                                row.names=F, col.names=F, quote=F, eol="")
        } }
        ### write the raw JSON data from the Twitter Firehouse to a text file (without locations)
        getURL("https://stream.twitter.com/1/statuses/filter.json",
                userpwd=userpwd,
                write = WRITE_TO_FILE,
                postfields = track,
                .opts = list(timeout = time, verbose = TRUE))
}

#==============================================================================
# PARSE TWEETS IN JSON TO DATA.FRAME
#==============================================================================

JSONtoDF <- function(JSONfile){
        require(rjson)
## Read the text file          
f <- JSONfile

## Function to make the reading process robust
## Source: http://stackoverflow.com/questions/8889017/handle-rjson-error-when-parsing-twitter-api-in-r
convertTwitter <- function(x) {
  ## ?Control
  z <- try(fromJSON(x))
  if(class(z) != "try-error")  {
    return(z)
  }
}

lines <- readLines(f, warn=FALSE)
results.list <- lapply(lines[nchar(lines)>0], convertTwitter)
                
# Function to parse tweet information
parse.tweet <- function(var, list=list){
        values <- rep(NA, length(list))
        missing <- sapply((sapply(list, '[[', var)), is.null)
        values[missing==FALSE] <- unlist(sapply(list, '[[', var))
        return(values)
}

# Function to parse user information
parse.user <- function(user.var, list=list){
        values <- rep(NA, length(list))
        user <- sapply(list, '[', "user")
        missing <- sapply(sapply(user, '[', user.var), is.null)
        values[missing==FALSE] <- unlist(sapply(user, '[', user.var))
        return(values)
}

# Function to parse location
parse.place <- function(place.var, list=list){
        values <- rep(NA, length(list))
        place <- if (!is.null(sapply(list, '[', "place"))) sapply(list, '[', "place") else vector("list", length(list))
        missing <- sapply(sapply(place, '[[', place.var), is.null)
        values[missing==FALSE] <- unlist(sapply(place, '[[', place.var))
        return(values)
}

# Function to parse coordinates
parse.coordinates <- function(list=list){
        values <- matrix(NA, ncol=2, nrow=length(list))
        coord <- sapply(sapply(list, '[', "coordinates"), '[', "coordinates")
        missing <- as.character(sapply(sapply(coord, '[[', "coordinates"), is.null))
        values[missing=="FALSE"] <- matrix(as.character(unlist(coord)[unlist(coord)!="Point"]), ncol=2, byrow=TRUE)
        return(values)
}


# Variables of interest, for each tweet and user
tweet.vars <- c("text", "retweet_count", "favorited", "truncated", "id_str", "in_reply_to_screen_name", "source", "retweeted", "created_at", "in_reply_to_status_id_str", "in_reply_to_user_id_str")
user.vars <- c("listed_count", "verified", "location", "id_str", "description", "geo_enabled", "created_at", "statuses_count", "followers_count", "favourites_count", "protected", "url", "name", "time_zone", "id", "lang", "utc_offset", "friends_count", "screen_name")
place.vars <- c("country_code", "country", "place_type", "full_name", "name", "id")


# Saves tweet and user information into memory
df.tweet <- as.data.frame(sapply(tweet.vars, parse.tweet, results.list, simplify=FALSE), stringsAsFactors=FALSE)
df.user <- as.data.frame(sapply(user.vars, parse.user, results.list, simplify=FALSE), stringsAsFactors=FALSE)
df.place <- as.data.frame(sapply(place.vars, parse.place, results.list, simplify=FALSE), stringsAsFactors=FALSE)
df.coord <- as.data.frame(parse.coordinates(results.list), stringsAsFactors=FALSE); names(df.coord) <- c("lon", "lat")


df <- cbind(df.tweet, df.user, df.place, df.coord)

cat(length(df$text), "tweets have been parsed")
return(df)
}

#==============================================================================
# CLEAN UNICODE STRING
#==============================================================================

clean.unicode <- function(variable){
unicode.errors <- c("\\\u0092", "\\\u0086", "\\\x8e", "\\\x8f", "\\\x84", "\\\x87", "\\\x88", "\\\x92", "\\\x96", "\\\x97", 
                                        "\\\xe7", "\\\xed", "\\\xbc", "\\\x9c", "\\\xf2", "\\\x86", "\\\xa1", "\\\x95", "\\\x9f", "\\\x9e",
                                        "\\#", "\\\x98", "\\\xf1", "\\\xec", "\\\x8d", "\\\U3e65653c", "\\\xc0", "'", "\\\xea", "\\\xbf",
                                        "\\\x8b", "\\\xab", "\\\xe1", "\\\U3e33383cc", "\\\x83ire/", "\\\xbb", "/", "\\\U3e33393c",
                                        "\\\x91", "\\\xc1", "\\\U3e33663c", "\\\xdc", "\\\xd1", "%", "&", "\\\x82", "\xed\xec", "\x8c", 
                                        "\\n", "\\t", "<U\\+[[:alnum:]]+>", "\\r", "\\\x8a", "\\\xc8", "\\\xc7", "\\\xb4", "\\\xa3", 
                                        "\\\xe8", "\\\xce", "\\\xc2")
unicode.corrected <- c("í", "á", "é", "é", "Ñ", "á", "ó", "í", "Ñ", "ó", "Á", "í", " ", "ú", "Ú", " ", " ", "i", "u", "u", "",
                                        "ò", "Ó", "I", "ç", "Ó", "¿", "", "Í", "o", "a", " ", " ", "É", " ", "a", " ", "í", "e", "i", "ó",
                                        "", "Í", " ", " ", "Ç", "", " ", " ", " ", "", "", "", "", "", "", "", "", "", "")
pb <- txtProgressBar(min=1,max=length(unicode.errors))
for (i in 1:length(unicode.errors)){ 
        variable <- gsub(unicode.errors[i], unicode.corrected[i], variable)
        setTxtProgressBar(pb, i)
}
return(variable)
}

#==============================================================================
# FORMAT TWITTER DATE IN POSIX
#==============================================================================

format.twitter.date <- function(datestring){
        datestring <- as.POSIXct(datestring, format="%a %b %d %H:%M:%S %z %Y")
        return(datestring)
}

#==============================================================================
# GET COORDINATES FROM A STRING
#==============================================================================

getCoordinates <- function(location, yahooID){
        require(RCurl)
        coord <- grep("-?[0-9]+\\.[0-9]+.*-?[0-9]+\\.[0-9]+", location)
        if (length(coord)>0){
        coordinates <- sub(x=location, pattern='.* (-?[0-9]{1,3}\\.[0-9]+,-?[0-9]{1,3}\\.[0-9]+).*', replacement="\\1")
        lat <-  sub(x=coordinates, pattern='.*(^-?[0-9]+\\.[0-9]+).*', replacement="\\1")
        lon <- sub(x=coordinates, pattern='.*,[[:blank:]]?(-?[0-9]{1,3}\\.[0-9]+).*', replacement="\\1")
        Encoding(lon) <- "UTF-8"; Encoding(lat) <- "UTF-8"
        if (nchar(lat)>12 | nchar(lon)>12){
            return(NA)
        }
        else {

        
                url <- paste("http://where.yahooapis.com/geocode?location=", lat, "+", lon, 
                "&gflags=R&appid=", yahooID, sep="")
                data.url <- getURL(url)
                ## read from xml
                tryCatch(doc <- xmlTreeParse(data.url, getDTD=F), error=function(e) e)
                if (exists("error")) { rm(error); next }
                tryCatch(r <- xmlRoot(doc), error=function(e) e)
                if (exists("error")) {rm(error); next}
                if (length(r)>5 & length(r)<8){ 
                    country <-  xmlValue(r[[6]][["country"]])
                    cat(location, ":", country, "\n")
                    if (length(country)>0) {return (country)} else {return(NA)}
                } else {return(NA)}
            }
        }
     else {return(NA)}
}


#==============================================================================
# GET COUNTRY FOR A GIVEN LOCATION
#==============================================================================

getYahooLoc <- function(location, yahooID){
    require(RCurl)
    url <- paste("http://where.yahooapis.com/geocode?q=", location, 
    "&ppid=", yahooID, sep="")
    url <- gsub(" ", "%20", url)   
    data.url <- getURL(url)
    return(data.url)
}

parseYahooLoc <- function(data.url){
    require(XML)
    tryCatch(doc <- xmlTreeParse(data.url, getDTD=F), error=function(e) e)
    if (exists("error")) { 
        return(NA) } 
    else {
        tryCatch(r <- xmlRoot(doc), error=function(e) e)
        if (exists("error")) { 
            return(NA) }    
        else {
            return(r)
        }
    }
}

getCountry <- function(location, yahooID){
    data.url <- getYahooLoc(location=location, yahooID=yahooID)
    r <- parseYahooLoc(data.url)
    if (length(r)>5 & length(r)<8){ 
        country <-  as.character(xmlValue(r[[6]][["country"]]))
        cat(location, ":", country, "\n")
        if (length(country)>0){
            return(country)
        }
        else { return(NA)}
    }
    else { 
        return(NA) }
}

#==============================================================================
# MATCH LOCATION STRINGS WITH SAVED MATCHES LOCATION-COUNTRY
#==============================================================================

getMatches <- function(location, matches){
    if (location %in% matches$location){
        country <- matches$country[which(matches$location %in% location)[1]]
        return(country)
    }
    else {
        return(NA)
    }
}

