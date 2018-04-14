library("ape")
library("Kmisc")

dat <- scan( what=character(), sep="\n", "rosalind_nwck.txt" )
trees <- dat[ seq(1, by=2, length.out=length(dat)/2) ]
nodes <- dat[ seq(2, by=2, length.out=length(dat)/2) ]

for( tree in trees ) {
  tree <- read.tree( textConnection( trees[i] ) )
  node <- us( nodes[i], " " )
  cat( cophenetic(tree)[node[1], node[2]], " " )
}