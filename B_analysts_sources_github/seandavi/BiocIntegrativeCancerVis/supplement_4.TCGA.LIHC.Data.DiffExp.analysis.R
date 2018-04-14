#
#   Differential expression analysis of paired TCGA LIHC RNASeqV2,
#   miRNAseqV2, and methylation data
#
#   References:
#   https://www.bioconductor.org/packages/release/bioc/html/edgeR.html
#   https://www.bioconductor.org/packages/release/bioc/html/limma.html
#   
#   Last updated on May 11, 2016 by Henry Zhang (hzhang@mail.nih.gov)
#   ____________________________________________________________________
#   <differential expression analysis><differential expression analysis>


library(ggplot2)
library(edgeR)

filePath <- "TCGAbiolinks.Download/"
outPath <- "TCGA.LIHC.Data.Processed/"
dir.create(outPath);


#   1.  Normalization of RNAseqRNAseq raw counts and DE analysis
#   ============================================================

    load(paste0(filePath,"TCGAbiolinks.LIHC.RNAseqV2.Raw.Counts.RData"));

    RNAseq <- as.matrix(TCGA.LIHC.RNAseqV2.Raw.Reads);
    colnames(RNAseq) <- substr(colnames(RNAseq), 1, 16)

    sampleToRemove <- c("TCGA-BC-A10X", "TCGA-BC-A110");
    nonKeep <- c(grep(sampleToRemove[1], colnames(RNAseq)), 
                grep(sampleToRemove[2], colnames(RNAseq)))
    RNAseq <- RNAseq[, -nonKeep]
    totalSamples <- ncol(RNAseq)
    totalPairs <- totalSamples/2

    sampleOrder <- order(colnames(RNAseq));
    RNAseq <- as.matrix(RNAseq[, sampleOrder]);

    groupOrder <- c(seq(1, totalSamples, by=2), seq(2, totalSamples, by=2));
    RNAseq <- RNAseq[, groupOrder];

    #   Remove the genes with very low counts 
    #   (keep genes with cpm > 1 in at least half samples)
    #
    keep <- rowSums(cpm(RNAseq) > 1) >= totalPairs
    RNAseq <- RNAseq[keep, ]

    design <- model.matrix(~ 0 + factor(c(rep(1,totalPairs),rep(2,totalPairs))))
    colnames(design) <- c("LiverHCC", "Normal");
    rownames(design) <- colnames(RNAseq);

    RNAseq <- DGEList(counts=RNAseq)
    RNAseq <- calcNormFactors(RNAseq)
    normalized.TCGA.LIHC.RNAseq.Counts <- voom(RNAseq,design)

    fileName <- paste0(outPath,"Normalized.TCGA.LIHC.RNAseqV2.Counts")
    save(normalized.TCGA.LIHC.RNAseq.Counts, file=paste0(fileName, ".RData"))
    write.table(signif(normalized.TCGA.LIHC.RNAseq.Counts$E, digits=5), 
                sep="\t", quote=FALSE, row.names=TRUE, col.names=TRUE,
                file=paste0(fileName, ".txt"))

    pca <- prcomp(normalized.TCGA.LIHC.RNAseq.Counts$E);
    groupColor <- c(rep("LiverHCC", totalPairs), rep("Normal", totalPairs))
    ggplot(as.data.frame(pca$rotation), 
            aes(x=pca$rotation[,1], y=pca$rotation[,2])) + 
            geom_point(aes(color=groupColor), size=4) + 
            labs(x="First PC", y="Second PC") +
            ggtitle("Normalized TCGA LIHC RNASeqV2 Sample PCA Plot")

    #   Differential expression analysis
    #
    contrast.matrix <- makeContrasts(LiverHCC-Normal, levels=design)
    fit1 <- lmFit(normalized.TCGA.LIHC.RNAseq.Counts$E, design)
    fit2 <- contrasts.fit(fit1, contrast.matrix)
    fit3 <- eBayes(fit2)

    TCGA.LIHC.RNAseqV2.lmfit.result <- topTable(fit3, coef=1, number=Inf)

    fileName <- paste0(outPath, "TCGA.LIHC.RNAseqV2.lmfit.result")
    save(TCGA.LIHC.RNAseqV2.lmfit.result, file=paste0(fileName, ".Rdata"))
    write.table(TCGA.LIHC.RNAseqV2.lmfit.result, sep="\t", quote=FALSE,
        row.names=TRUE, col.names=TRUE, file=paste0(fileName, ".txt"))


#   2.  Normalization of miRNA raw counts and DE analysis
#   =========================================================

    load(paste0(filePath, "TCGAbiolinks.LIHC.miRNAseqV2.Raw.Counts.RData"))

    miRNAseq <- as.matrix(TCGA.LIHC.miRNA.Raw.Reads)
    colnames(miRNAseq) <- substr(colnames(miRNAseq), 1, 16)

    sampleToRemove <- c("TCGA-BC-A10X", "TCGA-BC-A110");
    nonKeep <- c(grep(sampleToRemove[1], colnames(miRNAseq)), 
                grep(sampleToRemove[2], colnames(miRNAseq)))
    miRNAseq <- miRNAseq[, -nonKeep]

    totalSamples <- ncol(miRNAseq)
    totalPairs <- totalSamples/2

    #   Remove the miRNAs with very low counts 
    #   (keep miRNAs with cpm > 1 in at least half samples)
    #
    keep <- rowSums(cpm(miRNAseq) > 1) >= totalPairs
    miRNAseq <- as.matrix(miRNAseq[keep, ])

    miRNAseq <- miRNAseq[, order(colnames(miRNAseq))]
    sampleOrder <- c(seq(1, totalSamples, by=2), seq(2, totalSamples, by=2))
    miRNAseq <- miRNAseq[, sampleOrder]

    design <- model.matrix(~0 + factor(c(rep(1, totalPairs),rep(2,totalPairs))))
    colnames(design) <- c("LiverHCC", "Normal");
    rownames(design) <- colnames(miRNAseq);

    miRNAseq <- DGEList(counts=miRNAseq)
    miRNAseq <- calcNormFactors(miRNAseq)
    normalized.TCGA.LIHC.miRNA.Counts <- voom(miRNAseq, design)

    fileName <- paste0(outPath,"Normalized.TCGA.LIHC.miRNAseqV2.Counts")
    save(normalized.TCGA.LIHC.miRNA.Counts, file=paste0(fileName,".RData"))
    write.table(signif(normalized.TCGA.LIHC.miRNA.Counts$E), sep="\t", 
        quote=FALSE, row.names=TRUE, col.names=TRUE, 
        file=paste0(fileName,".txt"))

    pca <- prcomp(normalized.TCGA.LIHC.miRNA.Counts$E);
    group <- c(rep("LiverHCC", totalPairs), rep("Normal", totalPairs))
    ggplot(as.data.frame(pca$rotation), 
        aes(x=pca$rotation[, 1], y=pca$rotation[,2])) + 
        geom_point(aes(color=group), size=4) + 
        labs(x="First PC", y="Second PC") +
        ggtitle("Normalized TCGA LIHC miRNASeqV2 Sample PCA Plot")

    #   Differential expression analysis
    #
    contrast.matrix <- makeContrasts(LiverHCC-Normal, levels=design)
    fit1 <- lmFit(normalized.TCGA.LIHC.miRNA.Counts$E, design)
    fit2 <- contrasts.fit(fit1, contrast.matrix)
    fit3 <- eBayes(fit2)

    TCGA.LIHC.miRNAseqV2.lmfit.result <- topTable(fit3, coef=1, number=Inf)

    fileName <- paste0(outPath,"TCGA.LIHC.miRNAseqV2.lmfit.result")
    save(TCGA.LIHC.miRNAseqV2.lmfit.result, file=paste0(fileName, ".Rdata"))
    write.table(TCGA.LIHC.miRNAseqV2.lmfit.result, sep="\t", quote=FALSE,
        row.names=TRUE, col.names=TRUE, file=paste0(fileName, ".txt"))

#

#   3.  Methylation data
#   ===============================================================

    load(paste0(filePath, "TCGAbiolinks.LIHC.Methylation.beta.Values.RData"))

    annotation <- lihc.methylation.data[, c(1:3)]
    methylation <- as.matrix(lihc.methylation.data[, -c(1:3)]);
    rownames(methylation) <- paste(rownames(methylation),
            annotation[,1], sep="|")
    colnames(methylation) <- substr(colnames(methylation), 1, 16)

    sampleToRemove <- c("TCGA-BC-A10X", "TCGA-BC-A110");
    nonKeep <- c(grep(sampleToRemove[1], colnames(methylation)), 
                grep(sampleToRemove[2], colnames(methylation)))
    methylation <- methylation[, -nonKeep]

    totalSamples <- ncol(methylation)
    totalPairs <- totalSamples/2

    #   Remove the rows without gene matched and the rows 
    #   with missing values
    #
    noSymbols <- which(annotation$Gene_Symbol == "")
    methylation <- methylation[-noSymbols, ]

    keep <- complete.cases(methylation)
    methylation <- methylation[keep,]
    
    methylation <- methylation[, order(colnames(methylation))]
    sampleOrder <- c(seq(1, totalSamples, by=2), seq(2, totalSamples, by=2))
    methylation <- methylation[, sampleOrder]

    normalized.TCGA.LIHC.Methylation.Values <- normalizeBetweenArrays(
            methylation, method="quantile")

    fileName <- paste0(outPath,"Normalized.TCGA.LIHC.Methylation.Values")
    save(normalized.TCGA.LIHC.Methylation.Values, 
        file=paste0(fileName,".RData"))
    write.table(signif(normalized.TCGA.LIHC.Methylation.Values), 
        sep="\t", quote=FALSE, row.names=TRUE, col.names=TRUE, 
        file=paste0(fileName,".txt"))

    pca <- prcomp(normalized.TCGA.LIHC.Methylation.Values);
    group <- c(rep("LiverHCC", totalPairs), rep("Normal", totalPairs))
    ggplot(as.data.frame(pca$rotation), 
        aes(x=pca$rotation[, 1], y=pca$rotation[,2])) + 
        geom_point(aes(color=group), size=4) + 
        labs(x="First PC", y="Second PC") +
        ggtitle("Normalized TCGA LIHC Methylation Sample PCA Plot")

    #   Experiment design for DE analysis
    #
    design <- model.matrix(~ 0 + factor(c(rep(1, totalPairs), 
                rep(2, totalPairs))));
    colnames(design) <- c("LiverHCC", "Normal");
    rownames(design) <- colnames(normalized.TCGA.LIHC.Methylation.Values);

    #   Differential expression analysis
    #
    contrast.matrix <- makeContrasts(LiverHCC-Normal, levels=design)
    fit1 <- lmFit(normalized.TCGA.LIHC.Methylation.Values, design)
    fit2 <- contrasts.fit(fit1, contrast.matrix)
    fit3 <- eBayes(fit2)

    TCGA.LIHC.Methylation.lmfit.result <- topTable(fit3, coef=1, number=Inf)

    fileName <- paste0(outPath,"TCGA.LIHC.Methylation.lmfit.result")
    save(TCGA.LIHC.miRNAseqV2.lmfit.result, file=paste0(fileName, ".Rdata"))
    write.table(TCGA.LIHC.Methylation.lmfit.result, sep="\t", quote=FALSE,
        row.names=TRUE, col.names=TRUE, file=paste0(fileName, ".txt"))


#   End of TCGA.LIHC.Data.DiffExp.Analysis.RData
#   ========================================================================















#   End of TCGA.LIHC.RNAseq.miRNAseq.DiffExp.analysis.RData
#   ==========================================================================