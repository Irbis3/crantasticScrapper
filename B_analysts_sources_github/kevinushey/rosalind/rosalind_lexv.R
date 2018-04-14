setwd( paste0( dropbox_folder, "/rosalind/lexv" ) )

dat <- scan( what=character(), sep='\n', "rosalind_lexv.txt" )
dict <- unlist( strsplit( dat[1], " " ) )
names(dict) <- 1:length(dict)

n <- as.numeric(dat[2])

kmer <- function( dict, k, sort=FALSE ) {
  if( sort ) dict <- sort(dict)
  do.call( paste0, do.call( expand.grid, rep( list(dict), k ) )[k:1] )
}

alphabets <- vector("list",n)

for( i in 1:n ) {
  alphabets[[i]] <- kmer( dict, i )
}

lex_value <- function(x, dict) {
  n <- length(dict)+1
  tmp <- strsplit( x, "" )
  vals <- sapply( tmp, function(xx) {
    vals <- gsub( " ", "0", xx )
    vals <- as.numeric( kReplace( vals, dict ) )
    return( sum(n^( (length(vals)-1):0 ) * vals) )
  } )
  
  return( vals )
}

sort_alphabets <- function(alphabets, dict) {
  
  alphabet <- unlist( sapply( alphabets, function(x) {
    lx <- length(x)
    x <- paste0( x, paste0( rep(" ", n-nchar(x[1])), collapse="" ) )
    x
  }) )
  
  lex_vals <- lex_value(alphabet, dict)
  names( lex_vals ) <- alphabet
  return( gsub( " ", "", names( sort(lex_vals) ) ) )
  
}

cat( sort_alphabets( alphabets, dict ), sep="\n", file="out.txt" )
