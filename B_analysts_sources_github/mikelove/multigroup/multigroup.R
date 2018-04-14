######################################################
# grab some GTEx samples. files are publicly available

pheno <- read.delim("GTEx_Analysis_Annotations_Sample_DS__Pilot_2013_01_31.txt")
genecounts <- read.delim("GTEx_Analysis_RNA-seq_RNA-SeQCv1.1.8_gene_reads__Pilot_2013_01_31_patch1.gct",skip=2)
pheno$cleanid <- make.names(pheno$SAMPID)
# which samples correspond to RNA-Seq (some are exon array)
sequenced <- pheno$SMGEBTCHT == "TrueSeq.v1"
table(sequenced)
# which samples are in the gene counts table
ingenecounts <- pheno$cleanid %in% colnames(genecounts)
table(ingenecounts)
# build a list of samples for tissues
tissuecompare <- list()
n <- 5 # samples per tissue
tissues <- c("Blood","Brain","Heart","Lung","Muscle","Skin")
for (i in seq_along(tissues)) {
  tissuecompare[[i]] <- as.character(head(pheno[pheno$SMTS == tissues[i]
                                                & sequenced & ingenecounts,"cleanid"],n))
}
# the first two rows of the gene count is the name and description
idx <- match(unlist(tissuecompare), colnames(genecounts))
# extract only these columns specified above
genecounts.sub <- genecounts[,idx]
rownames(genecounts.sub) <- genecounts$Name
tissue <- factor(pheno$SMTS[match(colnames(genecounts.sub), pheno$cleanid)])
rs <- rowSums(genecounts.sub)
#hist(log10(rs+1),col="grey",breaks=40)
rowidx <- rs > 10
table(rowidx)





#################
# DESeq2 analysis

library(DESeq2)

# build dataset
dds <- DESeqDataSetFromMatrix(
  countData = genecounts.sub[rowidx,],
  colData = DataFrame(sample=colnames(genecounts.sub), t=tissue),
  design = ~ t)

# run the DESeq2 pipeline
# 32k genes x 30 samples, 6 conditions, takes 1.5 min
dds <- DESeq(dds)

# extract different results tables
# one tissue's effect size from the average
res1 <- results(dds,name="tMuscle",lfcThreshold=1)

# a contrast of one tissue against another
res2 <- results(dds,contrast=c("t","Heart","Muscle"),lfcThreshold=1)

# a contrast of 2 tissues against 4 tissues
res3 <- results(dds,contrast=list(c("tHeart","tMuscle"),
                      c("tBlood","tBrain","tLung","tSkin")),
                listValues=c(1/2,-1/4),
                lfcThreshold=1)





#######################################################
# some plots, using the plotit() function defined below

pdf("gtex_one.pdf",width=6,height=5)
par(mar=c(5,5,1,1))
plotit(res1,arrowcol=c("grey50","grey50","grey50","grey50","orange","grey50"))
dev.off()
png("gtex_one_ma.png")
plotMA(res1,ylim=c(-10,10))
dev.off()

pdf("gtex_contrast.pdf",width=6,height=5)
par(mar=c(5,5,1,1))
plotit(res2,c("grey50","grey50","orange","grey50","dodgerblue","grey50"))
dev.off()
png("gtex_contrast_ma.png")
plotMA(res2,ylim=c(-10,10))
dev.off()

pdf("gtex_contrast_2_4.pdf",width=6,height=5)
par(mar=c(5,5,1,1))
plotit(res3,c("dodgerblue","dodgerblue","orange","dodgerblue","orange","dodgerblue"))
dev.off()
png("gtex_contrast_2_4_ma.png")
plotMA(res3,ylim=c(-10,10))
dev.off()

### draw the intercept and tissue specific fold changes
plotit <- function(res,arrowcol) {
  resSort <- res[order(-res$stat),]
  gene <- rownames(resSort)[1]
  intercept <- coef(dds)[gene,1]
  cond <- coef(dds)[gene,-1]
  condSE <- coef(dds,SE=TRUE)[gene,-1]
  plotCounts(dds, gene, "t", transform=TRUE, main="")
  abline(h=intercept, lwd=2, col="green", lty=2)
  x <- 1:6 + .2
  qn <- qnorm(.975)
  segments(x, intercept, x, intercept+cond, col=arrowcol, lwd=2)
  points(x, intercept+cond, col=arrowcol, lwd=2)
  arrows(x, intercept+cond, x, intercept+cond+qn*condSE, col=arrowcol, lwd=2, angle=90, length=.075)
  arrows(x, intercept+cond, x, intercept+cond-qn*condSE, col=arrowcol, lwd=2, angle=90, length=.075)
}
