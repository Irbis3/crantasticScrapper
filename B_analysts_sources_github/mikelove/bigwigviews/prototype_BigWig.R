library(GenomicRanges)
library(BiocParallel)
library(rtracklayer)
library(genefilter)

fls <- c(
"wgEncodeCshlLongRnaSeqA549CellLongnonpolyaMinusRawSigRep1.bigWig",
"wgEncodeCshlLongRnaSeqA549CellLongnonpolyaMinusRawSigRep2.bigWig",
"wgEncodeCshlLongRnaSeqA549CellLongnonpolyaPlusRawSigRep1.bigWig",
"wgEncodeCshlLongRnaSeqA549CellLongnonpolyaPlusRawSigRep2.bigWig") 
grp <- factor(c(1,1,2,2))

gr <- GRanges("chr1",IRanges(11:20 * 1e6 + 1,width=1e4))

## low-level helpers that can be connected in
## modular way ...
onefile_views <- function(fl, selection=gr, reduce=TRUE) {
    v <- Views(import(fl, selection=gr, asRle=TRUE), ranges(gr))
    if (reduce)
        v[[seqlevels(selection)]]
    else
        v
}

onefile_vector <- function(fl, gr) {
    v <- onefile_views(fl, gr, FALSE)
    as(v[[seqlevels(gr)]][[1]], "numeric")
}

multifile_matrix <- function(gr, fls) {
    simplify2array(bplapply(fls, onefile_vector, gr))
}

ttest <- function(gr, fls, grp) {
    xx <- multifile_matrix(gr, fls)
    rowttests(xx, grp) 
}

## coverage by file: 
## Returns list same length as # files. This would
## be the data manip in delegateByFile().
res <- bplapply(fls, onefile_views, selection=gr) 

## ttest by range:
## Returns list same length as ranges. This would
## be the data manip in delegateByRange().
## Non-missing data would be consolidated and
## presented as metadata cols of the original
## GRanges. 
grlist <- split(gr, seq_along(gr)) 
res <- bplapply(grlist, ttest, fls, grp)


