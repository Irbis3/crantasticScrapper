require(httr)
require(XML)
require(RCurl)

# Add lat-long information based on place names:
# http://stackoverflow.com/questions/3257441/geocoding-in-r-with-google-maps


library(RCurl)
library(RJSONIO)

construct.geocode.url <- function(address, return.call = "json", sensor = "false") {
  root <- "http://maps.google.com/maps/api/geocode/"
  u <- paste(root, return.call, "?address=", address, "&sensor=", sensor, sep = "")
  return(URLencode(u))
}

gGeoCode <- function(address,verbose=FALSE) {
  if(verbose) cat(address,"\n")
  u <- construct.geocode.url(address)
  doc <- getURL(u)
  x <- fromJSON(doc,simplify = FALSE)
  if(x$status=="OK") {
    lat <- x$results[[1]]$geometry$location$lat
    lng <- x$results[[1]]$geometry$location$lng
    return(c(lat, lng))
  } else {
    return(c(NA,NA))
  }
}



## Download all images
get_fish_pages <- function(family){
  base <- "http://pbs.bishopmuseum.org/images/JER/"
  a <- GET(paste(base, "images.asp?nm=", family, "&loc=&size=i&cols=0", sep=""))
  b <- htmlParse(a)
  node <- getNodeSet(b, "//@href")
}


download_images <- function(node, base ="http://pbs.bishopmuseum.org/images/JER/", dest="./"){
  sapply(4:length(node), function(i){
    id <- as.character(gsub(".*ID=(-*\\d.+)", "\\1", node[[i]]))
    try(download.file(paste(base, "large/", id, ".jpg", sep=""), paste(dest, id, ".jpg", sep="")))
  })
}


get_metadata <- function(node, base ="http://pbs.bishopmuseum.org/images/JER/", geocode=TRUE){
  handle <- getCurlHandle()
  dat <-  sapply(4:length(node), function(i){
    lat = NA
    long = NA
    id <- as.character(gsub(".*ID=(-*\\d.+)", "\\1", node[[i]]))
    img <- paste(base, node[[i]], sep="")
    page <- getURLContent(img, curl=handle)
    p <- strsplit(page[[1]], "\n")[[1]]
    p <- gsub("\t", "", p)
    p <- gsub("\r", "", p)
    j <- grep("Date:", p)
    date <- gsub(".*</font> (\\w.*)</font>.*", "\\1", p[j])
    j <- grep("Locality:", p)
    locality <- gsub(".*</font> (\\w.*)</font>.*", "\\1", p[j])
    j <- grep("Original ID:", p)
    species <- gsub(".*<i>(\\w.* \\w.*)</i>.*", "\\1", p[j])
    j <- grep("Size", p)
    TL <- as.numeric( gsub(".*font>.* (\\d.*) TL.*", "\\1", p[j]))
    SL <- as.numeric( gsub(".*font> (\\d.*) SL;.*", "\\1", p[j]))
    if(geocode){
      latlong <- gGeoCode(locality)
      lat = latlong[1]
      long = latlong[2]
    }
    Rdate <- as.Date(date, "%d %B %Y")
    list(species=species, locality=locality, TL=TL, SL=SL, date=date, latitude=lat, longitude=long, Rdate = Rdate, id=id)
  })
  as.data.frame(t(dat))
}


require(rfishbase)
data(fishbase)
families <- unique(fish_names(fish.data, "Family"))
for(family in families){
  pages <- get_fish_pages(family) 
  metadata <- get_metadata(pages)
  out <- data.frame(lapply(metadata, function(x) as.character(x)))
  write.csv(out, paste(family, ".csv", sep=""))
  download_images(pages)
}

## Consider plotting on a map? http://cran.r-project.org/web/packages/RgoogleMaps/vignettes/RgoogleMaps-intro.pdf

