setwd( paste0( dropbox_folder, "/rosalind/prob" ) )

dat <- scan( what=character(), sep='\n', quiet=TRUE, "rosalind_prob.txt" )
s <- dat[1]
gc_content <- mean( us( s ) %in% c("C", "G") )
A <- as.numeric( unlist( strsplit( dat[-1], " " ) ) )
