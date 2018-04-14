# Dean Attali
# Feb 28, 2014
# UBC STAT540

# lab 7 - RNA-seq differential expression analysis

# disclaimer: at the original time of submission, I have almost 0 idea what's going on in this
# code, there was no explanation in the seminar and I was too lazy to understand every single line. 

# load libraries and input 
library(edgeR)
dat <- read.table("bottomly_count_table.tsv", header = TRUE, row.names = 1)
des <- read.table("bottomly_phenodata.tsv", header = TRUE, row.names = 1)


### EdgeR

with(des, table(strain))

# create a 'group' object describing which group each sample belongs to
group <- factor(c(rep("1", 10), rep("2", 11)))

# this produces an object of type DGEList which can be manipulated in a
# similar way to any other list object in R
# DGEList has two components: a 'counts' matrix storing the count data,
# and a 'samples' dataframe storing the sample information
dge.glm <- DGEList(counts = dat, group = group)
str(dge.glm)

names(dge.glm)
dge.glm[["samples"]]
nrow(dge.glm[[1]])
ncol(dge.glm[[1]])

# create design matrix
design <- model.matrix(~group)

dge.glm.com.disp <- estimateGLMCommonDisp(dge.glm, design, verbose = TRUE)
dge.glm.trend.disp <- estimateGLMTrendedDisp(dge.glm.com.disp)
dge.glm.tag.disp <- estimateGLMTagwiseDisp(dge.glm.trend.disp, design)
# plot the tagwise dispersion against log2-CPM (counts per million)
plotBCV(dge.glm.tag.disp)

fit <- glmFit(dge.glm.tag.disp, design)
colnames(coef(fit))
lrt <- glmLRT(fit, coef = 2)
topTags(lrt)
tt.glm <- topTags(lrt, n = Inf)
class(tt.glm)
nrow(tt.glm$table[tt.glm$table$FDR < 0.01, ])
interestingSamples <- rownames(tt.glm$table[tt.glm$table$FDR < 1e-50, ])
cpm(dge.glm.tag.disp)[interestingSamples, ]
summary(de.glm <- decideTestsDGE(lrt, p = 0.05, adjust = "BH"))

# 451 genes are under-expressed in group 2 (DBA/2J) compared with group 1 (C57BL/6J),
# 35660 show no differences in expression while 425 genes are over-expressed.

# plotting the tagwise log fold changes against log-cpm
tags.glm <- rownames(dge.glm.tag.disp)[as.logical(de.glm)]
plotSmear(lrt, de.tags = tags.glm)
abline(h = c(-2, 2), col = "blue")




#### DESeq
library(DESeq)
# reading in the same count table data and grouping information
deSeqDat <- newCountDataSet(dat, group)
head(counts(deSeqDat))

deSeqDat <- estimateSizeFactors(deSeqDat)
sizeFactors(deSeqDat)
deSeqDat <- estimateDispersions(deSeqDat)
# plotting the estimated dispersions against the mean normalized counts
plotDispEsts(deSeqDat)

results <- nbinomTest(deSeqDat, levels(group)[1], levels(group)[2])
str(results)
plotMA(results)




##### Voom and limma
library(limma)
norm.factor <- calcNormFactors(dat)
dat.voomed <- voom(dat, design, plot = TRUE, lib.size = colSums(dat) * norm.factor)
