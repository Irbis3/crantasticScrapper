# the point of this script is to create
# a subset of transcripts for the simulation

library(GenomicFeatures)
library(BSgenome.Hsapiens.UCSC.hg19)

# using the same transcript database (TxDb) as the Geuvadis analysis
# otherwise you can use a different TxDb, with the line:
# txdb <- makeTxDbFromGFF("/path/to/genes.gtf")
txdb <- loadDb("data/genesStandard.sqlite")

# make some objects for later
# the exons of each transcript
# a data.frame linking tx to gene
ebt0 <- exonsBy(txdb, by="tx")
gene.ids <- keys(txdb, keytype="GENEID")
gene.ids <- gene.ids[gene.ids != ""]
txdf <- select(txdb, keys=gene.ids, columns=c("TXID","TXCHROM","TXNAME"), keytype="GENEID")

# subset to chr1
chrs <- "chr1" 
txdf <- txdf[txdf$TXCHROM %in% chrs,]

# how many transcripts per gene?
tab.tx <- table(txdf$GENEID)
round(table(tab.tx)/length(tab.tx), 2)
head(table(tab.tx))

# pick 300 from each of { 1, 2, and 3-5 } isoform genes
set.seed(1)
tab.idx <- c(sample(which(tab.tx == 1), 300),
             sample(which(tab.tx == 2), 300),
             sample(which(tab.tx > 2 & tab.tx < 6), 300))
idx <- txdf$GENEID %in% names(tab.tx[tab.idx])
sample.tx <- txdf$TXID[idx]
sample.tx.names <- txdf$TXNAME[idx]

write.table(txdf[idx,c("GENEID","TXNAME")], file="t2g-map", row.names=FALSE, col.names=FALSE, quote=FALSE, sep="\t")

# if quantifiers need a GTF file
# write(paste0('"',sample.tx.names,'"'), file="transcripts")
# system("grep -f transcripts genesStandard.gtf > transcripts.gtf")

# subset the exons by transcript object
gene.factor <- factor(txdf$GENEID[idx])
ebt <- ebt0[sample.tx]
names(ebt) <- sample.tx.names

# get the DNA sequence
dssl <- getSeq(Hsapiens, ebt)
dna <- unstrsplit(dssl)

# write the DNA sequence
library(Rsamtools)
writeXStringSet(dna, file="transcripts.fa")

# save various objects on the simulation
save(ebt, idx, txdf, dna, file="data/simulate.rda")
