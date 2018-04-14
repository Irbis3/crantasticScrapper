setwd( paste0( dropbox_folder, "/rosalind/pdst" ) )
x <- read.FASTA( "rosalind_pdst.txt" )

strdiff <- function(x, y) {
  sum( x != y ) / length(x)
}

mat <- matrix(0, nrow=length(x), ncol=length(x) )
for( i in 1:length(x) ) {
  for( j in 1:length(x) ) {
    mat[i,j] <- strdiff( us(x[[i]]), us(x[[j]]) )
  }
}

write.table( round( mat, 4 ), file="out.txt", row.names=F, col.names=F,
             sep=' ', quote=F )