setwd( dropbox_folder, "/rosalind/spec" )
mmt <- scan( what=character(), sep='\n', textConnection('A   71.03711
C   103.00919
D   115.02694
E   129.04259
F   147.06841
G   57.02146
H   137.05891
I   113.08406
K   128.09496
L   113.08406
M   131.04049
N   114.04293
P   97.05276
Q   128.05858
R   156.10111
S   87.03203
T   101.04768
V   99.06841
W   186.07931
Y   163.06333 '))

mmt <- as.data.frame( kSplit( gsub( " +", " ", mmt ), " " ),
                      stringsAsFactors=FALSE )
names(mmt) <- c("protein","weight")
mmt$weight <- as.numeric( as.character( mmt$weight ) )

save( mmt, file="../rosalind_mmt.rda" )

dat <- diff( scan( "rosalind_spec.txt" ) )
get_closest_match <- function(x,y) {
  diffs <- abs(x - y$weight)
  return( as.character(y$protein[ which(diffs == min(diffs)) ][1]) )
}

for( i in 1:length(dat) ) {
  cat( get_closest_match(dat[i], mmt) )
}