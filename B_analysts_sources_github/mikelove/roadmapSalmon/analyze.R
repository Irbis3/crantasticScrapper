library(readr)
samps <- read_tsv("samples_sub.tsv")
samps <- samps[c("Run_s","DONOR_ID_s","TISSUE_TYPE_s")]
colnames(samps) <- c("run","donor","tissue")

samps$tissue[samps$tissue == "Adipose Tissue"] <- "Adipose"
samps$tissue <- factor(samps$tissue)

files <- file.path("salmon",samps$run,"quant.sf")
names(files) <- samps$run

library(tximport)
txi.tx <- tximport(files, type="salmon", txOut=TRUE, reader=read_tsv)

library(EnsDb.Hsapiens.v79)
df <- transcripts(EnsDb.Hsapiens.v79, return.type="DataFrame")
tx2gene <- df[,c("tx_id","gene_id")]

txi <- summarizeToGene(txi.tx, tx2gene, ignoreTxVersion=TRUE)

# this could also have been done with a single line of code:
# txi <- tximport(files, type="salmon", tx2gene=tx2gene, reader=read_tsv, ignoreTxVersion=TRUE)
library(DESeq2)
dds <- DESeqDataSetFromTximport(txi, samps, ~tissue)
vsd <- vst(dds, blind=FALSE)
plotPCA(vsd, "tissue")
dat <- plotPCA(vsd, "tissue", returnData=TRUE)

library(rafalib)
library(RColorBrewer)

bigpar()
mypal <- c(brewer.pal(8, "Dark2"), brewer.pal(8, "Accent"))
palette(mypal)
spec.lvls <- c("Right Atrium","Left Ventricle","Right Ventricle","Psoas Muscle",
               "Esophagus","Gastric",
               "Sigmoid Colon","Small Intestine")
lvls <- c(spec.lvls, setdiff(levels(dat$tissue), spec.lvls))
dat$tissue <- factor(dat$tissue, lvls)

with(dat, plot(PC1, PC2, col=tissue,
               pch=as.integer(tissue), cex=2, asp=1))
with(dat, legend("bottomright", levels(tissue),
                 col=seq_along(levels(tissue)),
                 pch=seq_along(levels(tissue)),
                 ncol=3, inset=.02))

exclude <- c("Psoas Muscle","Right Ventricle","Right Atrium","Left Ventricle",
             "Small Intestine","Sigmoid Colon",
             "Aorta")

dat2 <- plotPCA(vsd[, !vsd$tissue %in% exclude], "tissue", returnData=TRUE)
dat2$tissue <- factor(dat2$tissue, lvls)

tab <- table(dat2$tissue)
with(dat2, plot(PC1, PC2, col=tissue,
                pch=as.integer(tissue), cex=1.5, asp=1))
with(dat2, legend("bottom", levels(tissue)[tab > 0],
                 col=seq_along(levels(tissue))[tab > 0],
                 pch=seq_along(levels(tissue))[tab > 0],
                 ncol=3, inset=.02))

