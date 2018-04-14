#' Fill in NAs with nearest non-NAs
#' Fills in NAs in a vector with the nearest non-NA value. Similar to zoo::na.locf(), but uses the nearest, rather than the previous value.
#' Answer to the question proposed by geoffjentry at http://stackoverflow.com/questions/10077415/replacing-nas-in-r-with-nearest-value/10081444#10081444
#' @param dat the vector with (potentially) NAs to be replaced
#' @return the vector with the NAs having been replaced by the nearest non-NA value
#' @author Jeffrey D. Allen \email{jeff.allen@@trestletechnology.net}
imputeNAs <- function(dat){
  
  if(is.null(dat)){
    return(NULL);
  }
  
  if (all(is.na(dat))){
    stop("Can't impute NAs if there are no non-NA values.")
  }
  
  if (!any(is.na(dat))){
    return(dat);
  }
  
  #starts with an NA (or multiple), handle these
  if (is.na(dat[1])){
    firstNonNA <- which(!is.na(dat))[1]
    dat[1:(firstNonNA-1)] <- dat[firstNonNA]
  }
  
  #ends with an NA (or multiple), handle these
  if (is.na(dat[length(dat)])){
    lastNonNA <- which(!is.na(dat))
    lastNonNA <- lastNonNA[length(lastNonNA)]
    dat[(lastNonNA+1):length(dat)] <- dat[lastNonNA]
  }
  
  #get the index of all NA values
  nas <- which(is.na(dat))

  #get the Boolean map of which are NAs, used later to determine which values can be used as a replacement, and which are just filled-in NA values
  namask <- is.na(dat)

  #calculate the maximum size of a run of NAs
  length <- getLengthNAs(dat);

  #the furthest away an NA value could be is half of the length of the maximum NA run
  #if there's a run at the beginning or end, then the nearest non-NA value could possibly be `length` away, so we need to keep the window large for that case.
  windowSize <- ceiling(length/2)

  #loop through all NAs
  for (thisIndex in nas){
    #extract the neighborhood of this NA
    neighborhood <- dat[(thisIndex-windowSize):(thisIndex+windowSize)]
    #any already-filled-in values which were NA can be replaced with NAs
    neighborhood[namask[(thisIndex-windowSize):(thisIndex+windowSize)]] <- NA

    #the center of this neighborhood
    center <- windowSize + 1

    #compute the difference within this neighborhood to find the nearest non-NA value
    delta <- center - which(!is.na(neighborhood))

    #find the closest replacement
    replacement <- delta[abs(delta) == min(abs(delta))]
    #in case length > 1, just pick the first
    replacement <- replacement[1]

    #replace with the nearest non-NA value.
    dat[thisIndex] <- dat[(thisIndex - (replacement))]
  }
  dat
}

runit.testFirstNA <- function(){
  x <- c(NA, 1:3)
  checkIdentical(imputeNAs(x), as.integer(c(1,1:3)))
}

runit.testLastNA <- function(){
  x <- c(1:3, NA)
  checkIdentical(imputeNAs(x), as.integer(c(1:3,3)))
}

runit.testFirstNARun <- function(){
  x <- c(NA, NA, NA, NA, 1:3)
  checkIdentical(imputeNAs(x), as.integer(c(1,1,1,1,1:3)))
}

runit.testLastNARun <- function(){
  x <- c(1:3, NA, NA, NA, NA, NA)
  checkIdentical(imputeNAs(x), as.integer(c(1:3,3,3,3,3,3)))
}

runit.testSingle <- function(){
  x <- c(1:3, NA, 1:3)
  checkIdentical(imputeNAs(x), as.integer(c(1:3,3,1:3)))
}

runit.testDouble <- function(){
  x <- c(1:3, NA, NA, 1:3)
  checkIdentical(imputeNAs(x), as.integer(c(1:3,3,1,1:3)))
}

runit.testMixed <- function(){
  x <- c(1:3, NA, 3:4, NA, NA, 5:6, NA, NA, NA, 7:8)
  checkIdentical(imputeNAs(x), as.integer(c(1:3,3, 3:4, 4, 5, 5:6, 6, 6, 7, 7:8)))
}

runit.testAllNA <- function(){
  x <- c(NA, NA, NA)
  checkException(imputeNAs(x))
}

runit.testNoNA <- function(){
  x <- c(1:3)
  checkIdentical(imputeNAs(x), 1:3)
}

runit.testEmpty <- function(){
  x <- c()
  checkIdentical(imputeNAs(x), NULL)
}


