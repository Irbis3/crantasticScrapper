setwd( paste0( dropbox_folder, "/rosalind/tree" ) )

dat <- scan( what=character(), sep="\n", "rosalind_tree.txt" )

n <- as.numeric(dat[1]); edges <- strsplit( dat[2:length(dat)], " ")

unlinked_nodes <- as.list( as.character( (1:n)[ 1:n %nin% as.numeric(unlist(edges)) ] ) )
edges <- c( edges, unlinked_nodes )

group_edges <- function( edges ) {
  
  j <- length(edges)
  
  out <- vector("list", length(edges))
  
  for( i in 1:length(out) ) {
    
    if( length(edges) == 0 ) {
      break
    }
    
    out[[i]] <- edges[[1]]
    edges <- edges[-1]
    j <- 1 
    
    while( j <= length(edges) ) {
      if( any( edges[[j]] %in% out[[i]] ) ) {
        out[[i]] <- c( out[[i]], edges[[j]] )
        edges <- edges[-j]
      } else {
        j <- j + 1
      }
    }
    
  }
  
  out <- out[ !sapply( out, is.null ) ]
  
}

out_new <- group_edges(edges)
out_old <- NULL 

while( !identical( out_old, out_new) ) {
  out_old <- out_new
  out_new <- group_edges(out_old)
} 

cat( length(out_new)-1, "\n\n" )