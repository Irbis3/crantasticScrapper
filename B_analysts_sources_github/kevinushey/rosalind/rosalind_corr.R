setwd("D:/dropbox/Dropbox/rosalind/corr/")

library("Kmisc")

dat <- scan( what=character(), "rosalind_corr.txt" )

dat_split <- strsplit( dat, "" )

dat_rc <- c(dat, sapply( dat, reverse_compliment) )
names(dat_rc) <- NULL

reads <- table(dat_rc)
good_reads <- names( reads[ reads >= 2 ] )
good_reads_list <- strsplit( good_reads, "" )

bad_reads <- reads[ reads == 1 ]
bad_reads <- names( bad_reads[ names(bad_reads) %in% dat ] )

for( read in bad_reads ) {
  x <- us(read)
  dists <- sapply( good_reads_list, function(xx) {
    length(xx) - sum( xx == x )
  })
  cat( paste0( 
    paste(x, collapse=""), 
    "->", 
    paste(good_reads_list[[which(dists==1)]], collapse=""),
    "\n"
    ) )
}