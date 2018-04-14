setwd( paste0(dropbox_folder, "/rosalind/revp") )
dat <- scan( what=character(), file("rosalind_revp.txt") )

x <- us( dat )

kReplace <- function( vec, orig, out=names(orig) ) {
  tmp <- out[ match( vec, orig ) ]
  tmp[ is.na(tmp) ] <- vec[ is.na(tmp) ]
  tmp
}

reverse_compliment <- function(x) {
  nucs <- c("A","C","G","T")
  paste( rev( kReplace( us(x), nucs, rev(nucs) ) ), collapse="" )
}

gen_palindromes <- function(x, len, file=stdout()) {
  for( i in 1:(length(x)-len+1) ) {
    tmp <- paste( x[i:(i+len-1)], collapse="" )
    if( tmp == reverse_compliment(tmp) ) {
      cat( i, len, "\n", file=file )
    }
  }
}

if( file.exists("out.txt") ) {
  file.remove("out.txt")
}

outFile <- file("out.txt", 'a')
for( i in c(4,6,8) ) {
  gen_palindromes( x, i, outFile )
}
close(outFile)