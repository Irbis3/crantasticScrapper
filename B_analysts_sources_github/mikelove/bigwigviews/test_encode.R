# some Encode RNA-Seq BigWigs each 160 Mb
ftpPath <- "ftp://hgdownload.cse.ucsc.edu/goldenPath/hg19/encodeDCC/wgEncodeCshlLongRnaSeq/"
ftpFiles <- c("wgEncodeCshlLongRnaSeqA549CellLongnonpolyaMinusRawSigRep1.bigWig",
              "wgEncodeCshlLongRnaSeqA549CellLongnonpolyaMinusRawSigRep2.bigWig",
              "wgEncodeCshlLongRnaSeqA549CellLongnonpolyaPlusRawSigRep1.bigWig",
              "wgEncodeCshlLongRnaSeqA549CellLongnonpolyaPlusRawSigRep2.bigWig")

localPath <- "/Users/michael/scripts/bigwigviews/testData"
library(rtracklayer)
fls <- paste0(localPath, "/", ftpFiles)

library(GenomicFileViews)
# construct a BigWigFileViews instance
gr <- GRanges("chr1",IRanges(11:20 * 1e6 + 1,width=1e4))
bwfv <- BigWigFileViews(filePaths=fls, fileRanges=gr)

# basic methods
bwfv
names(bwfv)
dim(bwfv)
dimnames(bwfv)
bwfv[1:2,2]

# coverage method
z <- coverage(bwfv)

# summary method
# works
gr <- GRanges("chr1",IRanges(11007000 + 0:1 * 100,width=100))
bwfv <- BigWigFileViews(filePaths=fls, fileRanges=gr)
z <- summary(bwfv)
unlist(z)

# catches error, returns zero
gr <- GRanges("chr1",IRanges(11007000 + 0:2 * 100,width=100))
bwfv <- BigWigFileViews(filePaths=fls, fileRanges=gr)
z <- summary(bwfv)
unlist(z)

# works, because we are going one range at a time
gr <- GRanges("chr1",IRanges(11007000 + 0:2 * 100,width=100))
bwfv <- BigWigFileViews(filePaths=fls, fileRanges=gr, byFile=FALSE)
z <- coverage(bwfv)
z <- summary(bwfv,type="mean")
z <- summary(bwfv,type="sd")



#############################
#
# pre-GenomicFileViews code:
#
##############################
 
# stream along the genomic ranges and calculate t tests
# this should also go and grab the scaling factor from the bigWigSamples DataFrame
t <- system.time({ts <- lapply(seq_len(nrow(bwfv)), function(i) {
  cvr <- coverageSingleRange(bwfv,i)
  t <- rleTTest(cvr, 1:2, 3:4)
  t
})})

print(t)

# subset as we are going making a dense matrix
gr <- tileGenome(c("chr1"=249e6),cut.last.tile.in.chrom=TRUE,tilewidth=1e5)
bwfv <- BigWigViews(bigWigPaths=fls, bigWigRanges=gr)
bwfv <- bwfv[101:110,]

# this gives the matrix of coverage
z <- intCoverageMatrix(bwfv)
print(object.size(z),unit="Mb")

# plot coverage across replicates
# idx <- rowSums(z) > 0
# plot(z[idx,1],z[idx,2],cex=.5)

