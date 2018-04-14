setwd( paste0( dropbox_folder, "/rosalind/long" ) )
dat <- scan( what=character(), sep="\n", "rosalind_long.txt" )

dat <- sapply( dat, DNAString )

while( i < length(dat) ) {
  
}