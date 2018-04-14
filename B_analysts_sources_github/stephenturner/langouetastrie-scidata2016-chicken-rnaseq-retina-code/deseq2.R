# Load libraries ----------------------------------------------------------

library(DESeq2)
library(Tmisc)
library(dplyr)
 
 
# Import & pre-process ----------------------------------------------------
 
## Import data from featureCounts
countdata <- read.table("counts.txt", header=TRUE, row.names=1)
nn(countdata)
 
## Remove first five columns (chr, start, end, strand, length)
countdata <- countdata[ ,6:ncol(countdata)]
 
## Remove cruft from filenames
gsub(".star.Aligned.out.sam", "", colnames(countdata))
colnames(countdata) <- gsub(".star.Aligned.out.sam", "", colnames(countdata))
 
## Convert to matrix
countdata <- as.matrix(countdata)
head(countdata)
 
## Import coldata from file
(coldata <- read.table("coldata.txt", header=TRUE, row.names=1))
 
# Is everything set up properly?
all(rownames(coldata) == colnames(countdata))
 
 
# Analysis with DESeq2 ----------------------------------------------------
 
## Create a coldata frame and instantiate the DESeqDataSet
dds <- DESeqDataSetFromMatrix(countData=countdata, colData=coldata, design=~condition)
dds
 
## Run the DESeq pipeline and write out normalized counts
dds <- DESeq(dds)
write.csv(counts(dds, normalized=TRUE), file="chickenretina-normalized-counts.csv")
 
## Regularized log transformation for clustering/heatmaps, etc
rld <- rlogTransformation(dds)
 
## Variable of interest for plotting
intvar <- colData(rld)$condition
 
# Colors for plots below
library(RColorBrewer)
(mycols <- brewer.pal(8, "Dark2")[1:length(unique(intvar))])
 
## Sample distance heatmap
sampleDists <- as.matrix(dist(t(assay(rld))))
library(gplots)
png("chickenretina-run1-qc-heatmap-samples.png", w=1000, h=1000, pointsize=20)
heatmap.2(as.matrix(sampleDists), key=F, trace="none", col=colorpanel(100, "black", "white"), ColSideColors=mycols[intvar], RowSideColors=mycols[intvar], margin=c(10, 10), main="Sample Distance Matrix")
dev.off()
 
## Principal components analysis
# DESeq2::plotPCA(rld, intgroup="condition")
rld_pca <- function (rld, intgroup = "condition", ntop = 500, colors=NULL, legendpos="bottomleft", main="PCA Biplot", textcx=1, ...) {
    require(genefilter)
    require(calibrate)
    require(RColorBrewer)
    rv = rowVars(assay(rld))
    select = order(rv, decreasing = TRUE)[seq_len(min(ntop, length(rv)))]
    pca = prcomp(t(assay(rld)[select, ]))
    fac = factor(apply(as.data.frame(colData(rld)[, intgroup, drop = FALSE]), 1, paste, collapse = " : "))
    if (is.null(colors)) {
        if (nlevels(fac) >= 3) {
            colors = brewer.pal(nlevels(fac), "Paired")
        }   else {
            colors = c("black", "red")
        }
    }
    pc1var <- round(summary(pca)$importance[2,1]*100, digits=1)
    pc2var <- round(summary(pca)$importance[2,2]*100, digits=1)
    pc1lab <- paste0("PC1 (",as.character(pc1var),"%)")
    pc2lab <- paste0("PC1 (",as.character(pc2var),"%)")
    plot(PC2~PC1, data=as.data.frame(pca$x), bg=colors[fac], pch=21, xlab=pc1lab, ylab=pc2lab, main=main, ...)
    with(as.data.frame(pca$x), textxy(PC1, PC2, labs=rownames(as.data.frame(pca$x)), cex=textcx))
    legend(legendpos, legend=levels(fac), col=colors, pch=20)
}
rld_pca(rld, colors=mycols, intgroup="condition")
png("chickenretina-run1-qc-pca.png", 1000, 1000, pointsize=20)
rld_pca(rld, colors=mycols, intgroup="condition", xlim=c(-150, 100))
dev.off()
 
 
# Get results -------------------------------------------------------------
 
## Want results:
# 1. E18 retina vs E08 retina (late-late vs early retinal development)
# 2. E18 retina vs E18 cornea (tissue-specific expression)
# 3. E18 retina vs E16 retina (late-late vs early-late retinal development)
 
## Build results sets
resultsNames(dds)
res <- list()
res$e18retina_e08retina <- results(dds, contrast=c("condition", "e18retina", "e08retina"))
res$e18retina_e18cornea <- results(dds, contrast=c("condition", "e18retina", "e18cornea"))
res$e18retina_e16retina <- results(dds, contrast=c("condition", "e18retina", "e16retina"))
 
## Reorder results frames by ascending FDR
res <- lapply(res, function(x) x[order(x$padj), ])
 
## Write results and plots in a loop
for (i in names(res)) {
  basefilename <- paste0(paste0("chickenretina-results-", i, "-"))
  write.csv(res[[i]], file=paste0(basefilename, "genelist.csv"))
}
