# Rev 1.2: added checks for empty-set inputs 
vsetdiff <-  function (x, y, multiple=TRUE) 
{
    x <- as.vector(x)
    y <- as.vector(y)
 # new code to check for empty sets  here
 if(!length(x)) return(NULL)
 if(!length(y)) return(x)
 # end new code
    xx <- x[!is.na(x)]
    xn <- x[is.na(x)]
    yy <- y[!is.na(y)]
    yn <- y[is.na(y)]
# removed the if(length) as redundant
 #   if (length(x) || length(y)) {
        if (!multiple) {
             difout <- unique( x[match(x, y, 0L) == 0L])   #original code plus output obj
#this tapply setup fails on NA, which is why any NA were separated out
              }else {
 # if the output of unlist() is length 0
# then difout <- xx  (foo[-0]  does naughty things)
              	tapout <- unlist( tapply(yy, yy, function(yyy) head(which(xx == yyy[1]), length(yyy) )   )  ) 
              	if(length(tapout)) difout<-xx[-tapout] else difout<- xx
              	ndif <- max(0,length(xn)-length(yn) )
              	difout<- c(difout, rep(NA,ndif) )
              }
#        } else  difout <- x
     return(difout)
}
