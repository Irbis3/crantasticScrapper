library( RCurl )

setwd( dropbox_folder, "/rosalind/mprt" )
dat <- scan( what=character(), sep='\n', quiet=TRUE, "rosalind_mprt.txt" )
url_base <- "http://www.uniprot.org/uniprot/"

glyco_re <- "(?=[N][^P][ST][^P])"

get_URL <- function(url) {
  curl <- getCurlHandle()
  tmp <- getURL(url, curl=curl)
  return( list(tmp, getCurlInfo(curl)) )
}

for( protein in dat ) {
  
  url <- paste0( url_base, protein, ".fasta" )
  url_info <- get_URL(url)
  while( !is.null( url_info[[2]]$redirect.url ) ) {
    url_info <- get_URL( url_info[[2]]$redirect.url )
  }
  
  fasta <- scan( what=character(), sep='\n', quiet=TRUE, textConnection(
    url_info[[1]]
  ) )
  
  fasta <- paste( fasta[2:length(fasta)], collapse="" )
  
  matches <- gregexpr( glyco_re, fasta, perl=TRUE )[[1]]
  
  if( matches[1] != -1 ) {
    cat( protein )
    cat( "\n" )
    cat( matches )
    cat( "\n" )
  }
  
}