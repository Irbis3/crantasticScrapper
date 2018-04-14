library(GenomicRanges)
library(Biostrings)
load("data/simulate.rda")
load("data/geuvadisFPKM.rda")

# using a gene-level FPKM distribution from Geuvadis
# to assign expression levels to the genes
# then randomly to the transcripts, using a flat dirichlet distribution
gene.factor <- factor(txdf$GENEID[idx])
ngene <- nlevels(gene.factor)
set.seed(1)
bins <- sample(length(h$counts), ngene, replace=TRUE,
               prob=h$counts / sum(h$counts))
gene.fpkm <- 10^runif(ngene, h$breaks[bins], h$breaks[bins+1])
rdiri <- function(n) {
  if (n == 1) return(1)
  x <- sort(runif(n-1))
  c(x[1], diff(c(x,1)))
}
gene.tab <- as.numeric(table(gene.factor))
gene.iso <- lapply(table(gene.factor), function(n) rdiri(n))
iso.fpkm <- rep(gene.fpkm, times=gene.tab) * unlist(gene.iso)

# suppose an experiment-wide target of 60 million reads
# (we will then only simulate ~2000 transcripts)
simMillion <- 60 
frag.length <- 250
reads <- unname(iso.fpkm) * pmax(width(dna) - frag.length, 0)/1e3 * simMillion
round(quantile(reads, 0:20/20))
# cap reads/tx at 20000 reads
reads[reads > 20000] <- 20000 

# differential expression for 10% of transcripts
# simply 2x or 1/2x reads for these transcripts
de.ratio <- 0.1
set.seed(1)
fold.change <- sample(c(1,0.5,2), length(dna), replace=TRUE, c(1-de.ratio,de.ratio/2,de.ratio/2))
table(fold.change)
# the format for polyester
fold_changes <- matrix(c(rep(1, length(dna)),
                         fold.change),
                       ncol=2)
rownames(fold_changes) <- txdf$TXNAME[idx]
save(fold_changes, file="data/fold_changes.rda")

# load the bias parameters estimated from Geuvadis samples
# note: these GC curves are estimated *after* removing
# the bias from random hexamer priming using Cufflinks VLMM
library(splines)
load("fitpar_all.rda")

samps <- read.delim("samples.txt")
samps <- samps[,c("Comment.ENA_RUN.","Factor.Value.population.","Performer")]
colnames(samps) <- c("id","pop","perf")
stopifnot(all(samps$id == names(fitpar)))

# create a matrix of GC content percentile x samples
# which gives relative probabilities of observing
# fragments (scaled such that max = 1).
gc.knots <- seq(from=.4, to=.6, length=3)
gc.bk <- c(0,1)
gcrate <- matrix(nrow=101, ncol=30)
for (j in 1:30) {
  loglambda <- model.matrix(~ns(0:100/100,knots=gc.knots,Boundary.knots=gc.bk)) %*% fitpar[[j]]$coefs$all[1:5]
  gcrate[,j] <- exp(loglambda)
  gcrate[,j] <- gcrate[,j] / max(gcrate[,j])
}

# we can plot these GC bias curves
# plot(0:100/100, gcrate[,1], type="n", ylim=c(0,1))
# for (j in 1:30) lines(0:100/100, gcrate[,j], col=rep(1:2, each=15)[j])

# store this matrix as a list
fragGCBiasData <- as.list(as.data.frame(gcrate))
# a function which takes GC content and returns relative probabilities
fragGCBias <- function(x, data) {
  idx <- as.integer(cut(x, breaks=0:100/100))
  data[idx]
}

# load Mike's fork of polyester which adds fragment GC-content bias
# (along with some other bookkeeping tweaks, allowing 0 reads, etc)
# https://github.com/mikelove/polyesterAlpineMs
# git clone git@github.com:mikelove/polyesterAlpineMs.git
library(devtools)
load_all("../polyesterAlpineMs")

# now run polyester

# one simulation perfectly *confounds* the true condition, 
# the differential expressed transcripts in  `fold_changes`,
# with the bias from sequencing centers, so it should really be bad
# for methods that can't remove fragment GC content bias.

# another simulation *balances* true condition and bias from sequencing centers

simTypes <- c("confounded","balanced")

# below I run polyester twice within each simulation:
# the first run is used to determine how many fragments each sample will get.
# because my fork of polyester flips a coin for each fragment based on
# its GC content, some samples will have more fragments than others.
# I calculate weights, then scale up the depth in the second run
# of polyester, which actually writes out FASTA files.
for (st in simTypes) {
  simsamps <- read.delim(paste0("sample_tables/",st,".csv"),sep=" ")
  # first run, to see how we should up-scale the library sizes to
  # get constant expected library size after fragGCBias
  bias.idx <- c(which(simsamps$batch == 1),
                which(simsamps$batch == 2))
  set.seed(1)
  lib.sizes.wts <- simulate_experiment(fasta="transcripts.fa",
                                       num_reps=c(15,15),
                                       reads_per_transcript=reads,
                                       fold_changes=fold_changes,
                                       weightsOnly=TRUE,
                                       size=100,
                                       readlen=100,
                                       distr="normal",
                                       fraglen=rep(frag.length,30),
                                       fragsd=rep(25,30),
                                       outdir=file.path("fasta",st),
                                       fragGCBias=fragGCBias,
                                       fragGCBiasData=fragGCBiasData[bias.idx],
                                       seed=1)
  save(lib.sizes.wts, file=file.path("data",st,"lib.sizes.wts.rda"))
  set.seed(1)
  simulate_experiment(fasta="transcripts.fa",
                      num_reps=c(15,15),
                      reads_per_transcript=reads,
                      fold_changes=fold_changes,
                      lib_sizes=lib.sizes.wts,
                      size=100,
                      readlen=100,
                      distr="normal",
                      fraglen=rep(frag.length,30),
                      fragsd=rep(25,30),
                      outdir=file.path("fasta",st),
                      fragGCBias=fragGCBias,
                      fragGCBiasData=fragGCBiasData[bias.idx],
                      seed=1)
}
  
# finally, we will need to shuffle these FASTA files
# in order to run Salmon or eXpress, because polyester
# writes out fragments one transcript at a time.

