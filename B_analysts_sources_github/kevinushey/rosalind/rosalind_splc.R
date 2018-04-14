library( Biostrings )

setwd( paste0( dropbox_folder, "/rosalind/splc" ) )
dat <- scan( what=character(), "rosalind_splc.txt" )
sequence <- dat[1]; substrs <- dat[2:length(dat)]
for( substr in substrs ) {
  sequence <- gsub( substr, "", sequence )
}

cat( as.character( translate( DNAString(sequence) ) ) )