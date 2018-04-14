source("~/.Rprofile")
setwd( paste0( dropbox_folder, "/rosalind/rear" ) )

dat <- scan( what=character(), sep="\n", "rosalind_rear.txt" )

d1 <- dat[seq(1, length(dat), by=2)]
d2 <- dat[seq(2, length(dat), by=2)]

out <- vector("list", length(d1))

for( i in seq_along(out) ) {
  
  s1 <- as.numeric( us(d1[i], " " ) )
  s2 <- as.numeric( us(d2[i], " " ) )
  
  out[[i]] <- data.frame( s1, s2 )
  
}

for( i in seq_along(out) ) {
  x <- out[[i]][,1]
  cat( ">Human\n", file="tmp.txt" )
  cat( x, file="tmp.txt", append=TRUE )
  system( paste( "grimm -f tmp.txt" ) )
}
