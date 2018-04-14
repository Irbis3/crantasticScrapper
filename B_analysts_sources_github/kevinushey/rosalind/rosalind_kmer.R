setwd( paste0( dropbox_folder, "/rosalind/kmer" ) )

dat <- read.FASTA( "rosalind_kmer.txt" )

dict <- c("A","C","G","T"); k <- 4

kmer <- function( dict, k ) {
  do.call( paste0, do.call( expand.grid, rep( list(dict), k ) )[k:1] )
}

alphabet <- kmer( dict, 4 )

for( x in alphabet ) {
  term <- paste0( "(?=", x, ")" )
  matches <- gregexpr( term, dat, perl=TRUE )
  cat( length( matches[[1]][ matches[[1]] != -1 ] ) )
  cat(" ")
}
