# codon_stats.R -- functions for looking at codon usage
if (!interactive())
  args <- commandArgs(TRUE)
if (!interactive() && length(args) < 2) {
  message("usage: Rscript codon_stats.R <codon_table.tsv> <sample size>\n\nerror: too few arguments.\n")
  message("This script takes a table from orf_codon_table.py and makes some plots.")
  message("Specifically, it produces plots looking at codon usage across \ndifferent groups (psuedogenes in our examples)")
  quit(status=1)
}

suppressMessages(library(ggplot2))
suppressMessages(library(Biostrings))
CODONS <- data.frame(codon=names(GENETIC_CODE), amino.acid=GENETIC_CODE)
CODONS.GROUP <- split(CODONS, list(CODONS$amino.acid))

freq2prop <- function(x)
  sweep(x, 1, rowSums(x), `/`)

scaledDist <- function(x)
  dist(t(scale(t(x))))

codonMDSDataframe <- function(dist, masking, type) {
  mds.fit <- cmdscale(dist, eig=TRUE, k=2)
  
  mds.d <- data.frame(x1=mds.fit$points[, 1], x2=mds.fit$points[, 2],
                      masking=masking, type=type)
  mds.d
}

main <- function(file, N=NULL) {
  message("reading data...")
  d <- read.delim(file, header=TRUE)
  # get all pseudogenes and a random number of non-pseudogenes
  # take random sample

  if (!is.null(N))
    message(sprintf("sampling data (n=%d for each group)...", N))
  d.pg <- local({
    # if our sample size is smaller, subsample this
    tmp <- subset(d, type != "NI")
    if (!is.null(N) && nrow(tmp) > N) {
      i <- sample(1:nrow(tmp), N)
      tmp <- tmp[i, ]
    }
    tmp
  })
  
  d.npg <- local({
    tmp <- subset(d, type == "NI")
    i <- sample(1:nrow(tmp), nrow(d.pg))
    tmp[i, ]
  })
  
  d.ss <- rbind(d.pg, d.npg)

  d.ss.prop <- freq2prop(d.ss[, -c(1:3)])
  message("calculating distance matrix...")
  D <- scaledDist(d.ss.prop)
  message("calculating MDS values and dataframe...")
  mds.d <- codonMDSDataframe(D, d.ss$masking, d.ss$type)
  message("creating plot (only contigs with no masking)...")
  p <- ggplot(subset(mds.d, masking == "none")) + geom_point(aes(x=x1, y=x2, color=type), alpha=0.6) + theme_bw()
  message("saving plot...")
  ggsave(p, file="codon.pdf", height=8, width=10)
}

if (!interactive()) {
  main(args[1], N=as.integer(args[2]))
}

## # fourfold degenerate sites
## tmp <- d.ss[, arginine.codons]
## all.zero <- apply(tmp == 0, 1, all)
## d.ss <- d.ss[!all.zero, ]
## d.ss.prop <- freq2prop(d.ss[, arginine.codons])
## d.ss.prop <- cbind(d.ss[, 1:3], d.ss.prop)
## D <- scaledDist(d.ss.prop)
## mds.d <- codonMDSDataframe(D, d.ss$masking, d.ss$type)
## ggplot(subset(mds.d, masking == "all")) + geom_point(aes(x=x1, y=x2, color=type), alpha=0.6) + theme_bw()

## codonUsagePlot <- function(d) {
##   tmp <- d[, -c(1:3)]
##   all.zero <- apply(tmp == 0, 1, all)
##   tmp.d <- d[!all.zero, -c(1:3)]
##   ## d.prop <- freq2prop(tmp.d[, -c(1:3)])
##   ## d.prop <- cbind(d[, 1:3], d.prop)

##   cdn <- melt(d)
##   cdn <- subset(cdn, masking == "all") # TODO CHANGE
##   cdn$amino.acid <- FOURFOLD$amino.acid[match(cdn$variable, FOURFOLD$codon)]
##   cdn <- na.exclude(cdn) # remove non-4-fold degenerate
##   grouping <- list(type=cdn$type, amino.acid=cdn$amino.acid)
##   totals <- aggregate(cdn$value, grouping, sum)

##   agg.d <- aggregate(cdn$value, list(type=cdn$type, codon=cdn$variable), sum)
##   agg.d$amino.acid <- FOURFOLD$amino.acid[match(agg.d$codon, FOURFOLD$codon)]

##   agg.d$key <- paste(agg.d$type, agg.d$amino.acid, sep="-")
##   totals$key <- paste(totals$type, totals$amino.acid, sep="-")
##   agg.d$total.key <- totals$x[match(agg.d$key, totals$key)]
##   agg.d$prop <- agg.d$x/agg.d$total.key
    
##   p <- ggplot(na.exclude(agg.d))
##   p <- p + geom_path(aes(x=codon, y=prop, group=type, color=type),
##                      stat="identity")
##   p + facet_wrap(~ amino.acid, scales="free_x")
## }

