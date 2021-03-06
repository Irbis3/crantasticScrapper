# Rev 1.2: added checks for empty-set inputs
vintersect <- function(x,y,multiple=TRUE){
	trueint <-intersect(x,y)
	 x <- as.vector(x)
    y <- as.vector(y)
     # new code to check for empty sets  here
     # make output look just like base::intersect 
 if(!length(x) | !length(y)) return( trueint )
 # end new code
    xx <- x[!is.na(x)]
    xn <- x[is.na(x)]
    yy <- y[!is.na(y)]
    yn <- y[is.na(y)]
    #unlike vdiff, here I want the difference in how many NA there are
    ndif <- min(length(xn), length(yn))
	if(multiple) {
		intout <- vector()
		for(jj in 1: length(trueint) ) {
			intout <- c(intout, rep(trueint[jj], min(length(which(trueint[jj]==xx)),length(which(trueint[jj]==yy) ) ) ) )
			}
		trueint<-c( intout, rep(NA,ndif))
		}
	return(trueint)
}
