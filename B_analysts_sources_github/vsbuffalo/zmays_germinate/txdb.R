## txdb.R -- transcript database, assumes being run from project root, from Makefile

## makeTxDb <- function(gff3.file, chrominfo, data.source, species) {
##     chrominfo.d <- read.delim(chrominfo, header=FALSE,
##                               colClasses=c("character", "integer", "logical"))

##     txdb <- makeTranscriptDbFromGFF(file=gff3.file,
##                                     format="gff3",
##                                     exonRankAttributeName="rank",
##                                     chrominfo=chrominfo.d,
##                                     dataSource=data.source,
##                                     species=species)
##     return(txdb)
## }

library(GenomicFeatures)

if (!interactive()) {
  args <- as.list(commandArgs(trailingOnly=TRUE))
} else {
  args <- "AGPv3.20"
}

version <- unlist(strsplit(gsub("AGPv(\\d+)\\.(\\d+)", "\\1;;;\\2", args[[1]]), ";;;"))

if (version[1] != "3" && version[2] != "20")
  error("Only Ensembl release 20 supported (see listMarts() in R, from the biomaRt package")

# we support different verions in the future
txdb <- makeTranscriptDbFromBiomart(biomart=sprintf("plants_mart_%s", version[2]), data="zmays_eg_gene")
saveDb(txdb, file = sprintf("data/%s/resources/zmays_txdb_ensembl_%s.sqlite", args[[1]], version[2]))

