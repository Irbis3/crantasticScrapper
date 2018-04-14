library(XML)
library(httr)
library(lubridate)

#' Download the bookings for a particular day.
downloadDate <- function(month, day, year){  
  dat <- GET("http://acm.co.lake.ca.us/sheriff/arrests.asp", query=list(ArrestDate=paste(month, day, year, sep="/")))
  if (dat$headers$status != "200"){
    # e.g. 12/03/2013 gives a 500 with an error on the page, but works just fine.
    warning ("Non-200 status returned from GET request to server:", month, "-", day, "-", year)
  }  
  con <- content(dat, as="text")  
  html <- htmlParse(con, asText=TRUE)
  tab <- readHTMLTable(html)
  if (length(tab) <= 2){
    tab <- tab[[1]]
  }
  legit <- which(grepl("^[[:digit:]]+$", as.character(tab[,1])))
  colnames(tab) <- as.character(unlist((tab[min(legit)-1,])))
  tab <- tab[legit,]
  tab
}

# Worried about putting too many pictures into a single dir. Split them up into 100
# directories by their last two digits.
# Also creates the dir if it doesn't exist.
getImagePath <- function(id){
  dir <- substr(id, nchar(id)-1, nchar(id))
  dir.create(paste0("data/pics/", dir), showWarnings = FALSE, recursive = TRUE)
  paste0("data/pics/",dir,"/",id,".jpg")
}

#' Download the details of the particular booking ID provided.
downloadBooking <- function(bookingID){
  dat <- GET("http://acm.co.lake.ca.us/sheriff/bookingdetail.asp", query=list(booknum=bookingID))
  if (dat$headers$status != "200"){
    stop ("Non-200 status returned from GET request to server: ", dat$headers)
  }
  
  con <- content(dat, as="text")
  html <- htmlParse(con, asText=TRUE)
  
  # Get the mugshot
  img <- xmlGetAttr(getNodeSet(html, path = "//img")[[1]], "src")
  # Get just the ID query
  if (length(img) >= 1 && img[1] != "images/nophoto.gif"){
    img <-  gsub(".*[?&]id=(\\d+)", "\\1", img[[1]], perl=TRUE)
    pic <- GET("http://acm.co.lake.ca.us/sheriff/getimage.asp", query=list(id=img))
    bin <- content(pic, "raw")
    writeBin(bin, getImagePath(bookingID))
  }
  
  tabs <- readHTMLTable(html)
  
  #Parse the individual tables in this document. They typically have the first row as
  # a section heder, then have alternating rows of titles and content which we'll want
  # to pair up in a list. The first one is usually empty, so start with the second.
  nameTrack <- processNameTrackingTable(tabs[[3]])
  phys <- processTable(NULL, tabs[[4]], header=FALSE)$data
  personal <- processTable(NULL, tabs[[5]], header=FALSE)$data
  arrest <- processTable(NULL, tabs[[6]], header=FALSE)$data
  courtChrg <- processTable(bookingID, tabs[[7]], header=FALSE, lastTable=TRUE)
  court <- courtChrg$data
  charges <- courtChrg$charges
  
  info <- list(NameTracking=nameTrack, Physical=phys, Personal=personal, Arrest=arrest,
               Court=court)
  
  return(list(data=info, charges=charges))
}

processNameTrackingTable <- function(table){
  # Trim extra links off the bottom
  table <- table[1:2,]
  processTable(NULL, table, header=FALSE)$data
}

#' Parse a table having a content header row then alternating rows of 
#' titles and content which we'll want to pair up in a list.
#' @param lastTable Whether or not this is the last table in the document. Currently, the
#' last table requires extra formatting as it's actually two separate tables and some
#' additional content at the end.
processTable <- function(bookingID, table, header=TRUE, lastTable=FALSE){
  firstTable <- FALSE
  
  
  if (!lastTable){
    if (firstTable){
      #trim to only second and third rows.
      table <- table[2:3,]
    } else if (header){    
      table <- table[-1,]
    }
    
    results <- list()
    for (i in 1:(nrow(table)/2)){
      labels <- as.character(unlist(table[(i*2-1),]))
      #remove trailing colons
      labels <- sub("(.*):", "\\1", labels)
      
      values <- as.character(unlist(table[(i*2),]))
      #Sometimes included in extraneous spacing
      values <- gsub("Â", "", values)
      
      theseResults <- as.list(values)
      names(theseResults) <- labels
      
      #Null holder in these tables is this character. If you see, ignore field.
      theseResults <- theseResults[names(theseResults) != "Â"]
      theseResults <- theseResults[!is.na(names(theseResults))]
      
      results <- c(results, theseResults)
    }
    return(list(data=results))
  }
  if (lastTable){
    table <- table[-nrow(table),]
    table <- table[table[,2] != "Â",]
    
    #Process first row (Court Information)
    results <- list()
    
    labels <- as.character(unlist(table[1,]))
    #remove trailing colons
    labels <- sub("(.*):", "\\1", labels)
    
    values <- as.character(unlist(table[2,]))
    #Sometimes included in extraneous spacing
    values <- gsub("Â", "", values)
      
    theseResults <- as.list(values)
    names(theseResults) <- labels
      
    #Null holder in these tables is this character. If you see, ignore field.
    theseResults <- theseResults[names(theseResults) != "Â"]
    theseResults <- theseResults[!is.na(names(theseResults))]
      
    results <- theseResults
    
    #Process all charges
    labels <- as.character(unlist(table[4,]))
    #remove trailing colons
    labels <- sub("(.*):", "\\1", labels)
    omit <- is.na(labels)
    labels <- labels[!omit]
    
    charges <- list()
    
    for (i in 5:nrow(table)){
      values <- as.character(unlist(table[i,]))
      #Sometimes included in extraneous spacing
      values <- gsub("Â", "", values)
      values <- values[!omit]
      
      theseResults <- as.list(c(bookingID, values))
      names(theseResults) <- c("BookingID", labels)
            
      #Null holder in these tables is this character. If you see, ignore field.
      theseResults <- theseResults[names(theseResults) != "Â"]      
      
      charges[[length(charges)+1]] <- theseResults
    
    }
    return(list(data=results, charges=charges))
  }
  
}


