setwd( paste0( dropbox_folder, "/rosalind/full" ) )

mtt <- get( load( "../rosalind_mmt.rda" ) )

x <- scan( textConnection("1988.21104821
610.391039105
738.485999105
766.492149105
863.544909105
867.528589105
992.587499105
995.623549105
1120.6824591
1124.6661391
1221.7188991
1249.7250491
1377.8200091") )

p <- x[1] 
dat <- sort( x[-1] )
k <- dat - min(dat)

## This function takes a vector of numbers, matches them
## in the 'mtt' and returns the matched letters.

matchNums <- function(x) {
  out <- rep(NA,length(x))
  for( i in seq_along(x) ) {
    xx <- x[i]
    found <- which( abs( xx - mmt$weight ) < 0.0001 ) 
    if( length( found ) > 0 ) {
      out[i] <- mtt$protein[found]
    }
  }
  return( out[!is.na(out)] )
}

matchNums(k)