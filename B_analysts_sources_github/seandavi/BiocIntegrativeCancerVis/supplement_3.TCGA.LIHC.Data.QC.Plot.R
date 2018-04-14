#
#   PCA and box plot of TCGA LIHC RNAseq, miRNAseq, methylation data
#   SNP/CNV and mutation data will not be analyzed since they should 
#   be sample specific for most data points
#
#   References:
#   https://www.bioconductor.org/packages/release/bioc/html/edgeR.html
#   https://www.bioconductor.org/packages/release/bioc/html/limma.html
#
#   Hadley Wickham: ggplot2, Use R. DOI 10.1007/978-0-387-98141-3, 
#   ©Springer Scientce+Business Media, LLC, New York. 2009.
#
#   Last updated on May 11, 2016 by Henry Zhang (hzhang@mail.nih.gov)
#   _________________________________________________________________
#   <General Visulization><General Visulization><General Visulization>


    library(ggplot2)
    library(edgeR)
    filePath <- "TCGAbiolinks.Download/"

#   1.  RNAseq. Data matrix is sorted by tissue type then sample ID
#   ===============================================================

    load(paste0(filePath, "TCGAbiolinks.LIHC.RNAseqV2.Raw.Counts.RData"))

    totalSamples <- ncol(TCGA.LIHC.RNAseqV2.Raw.Reads)
    totalPairs <- totalSamples/2
    
    #   sort the data first by sample name then by sample type
    #   to make sure samples are in the order of tumors followed
    #   by paired normal tissue.
    #
    colOrder <- order(colnames(TCGA.LIHC.RNAseqV2.Raw.Reads));
    RNAseq <- as.matrix(TCGA.LIHC.RNAseqV2.Raw.Reads[, colOrder]);

    sampleOrder <- c(seq(1, totalSamples, by=2), seq(2, totalSamples, by=2));
    RNAseq <- RNAseq[, sampleOrder];

    colnames(RNAseq) <- substr(colnames(RNAseq), 1, 16)
    dim(RNAseq) #   [1] 20531    56

    #   Remove the genes with very low counts 
    #   (cpm > 1 in at least half samples)
    #
    keep <- rowSums(cpm(RNAseq) > 1) >= totalPairs
    RNAseq <- RNAseq[keep, ]
    dim(RNAseq) #   [1] 12614    56

    #   Experiment design for DE analysis
    #
    design <- model.matrix(~ 0 + factor(c(rep(1, totalPairs), 
                    rep(2, totalPairs))));
    colnames(design) <- c("LiverHCC", "Normal");
    rownames(design) <- colnames(RNAseq);

    #   Data normalization 
    #
    RNAseq <- DGEList(counts=RNAseq)
    RNAseq <- calcNormFactors(RNAseq)
    normalized.TCGA.LIHC.RNAseq.Counts <- voom(RNAseq,design)

    RNAseq <- normalized.TCGA.LIHC.RNAseq.Counts$E
    colnames(RNAseq) <- substr(colnames(RNAseq), 1, 16)

    #   PCA plot
    #
    pca <- prcomp(RNAseq);

    #   There are two tumor samples sitting in normal tissue clusters.
    #   Manually check the xy coordinates for them then add labels
    #   which(pca$rotation[,2]>0)[1:2] 
    #   TCGA-BC-A10X-01A TCGA-BC-A110-01A
    #              6                9

    group <- c(rep("LiverHCC", totalPairs), rep("Normal", totalPairs))

    ggplot(as.data.frame(pca$rotation), 
        aes(x=pca$rotation[,1], y=pca$rotation[,2])) + 
        geom_point(aes(color=group), size=4) + 
        labs(x="First PC", y="Second PC") +
        ggtitle("TCGA LIHC RNASeqV2 Sample PCA Plot") +
        geom_text(label=colnames(RNAseq)[6], size=3,
            aes(x=pca$rotation[6,1]+0.005, y=pca$rotation[6,2]-0.02)) +
        geom_text(label=colnames(RNAseq)[9], size=3,
            aes(x=pca$rotation[9,1]-0.007, y=pca$rotation[9,2]+0.02))



#   2.  miRNAseq. Start form raw read counts
#   =========================================

    load(paste0(filePath, "TCGAbiolinks.LIHC.miRNAseqV2.Raw.Counts.RData"))
    dim(TCGA.LIHC.miRNA.Raw.Reads)  #   [1] 1046   56

    totalSamples <- ncol(TCGA.LIHC.miRNA.Raw.Reads)   
    totalPairs <- totalSamples/2

    #   sort the data first by sample name then by sample type
    #   to make sure samples are in the order of tumors followed
    #   by paired normal tissue.
    #
    colOrder <- order(colnames(TCGA.LIHC.miRNA.Raw.Reads));
    miRNAseq <- as.matrix(TCGA.LIHC.miRNA.Raw.Reads[, colOrder])
    
    sampleOrder <- c(seq(1, totalSamples-1, by=2), seq(2, totalSamples, by=2))
    miRNAseq <- miRNAseq[, sampleOrder]    

    #   Remove the miRNAs with very low counts 
    #   (cpm > 1 in at least half samples)
    #
    keep <- rowSums(cpm(miRNAseq) > 1) >= totalPairs
    miRNAseq <- as.matrix(miRNAseq[keep, ])
    colnames(miRNAseq) <- substr(colnames(miRNAseq), 1, 16)
    dim(miRNAseq)  #   [1] 320  56

    #   Experiment design for DE analysis
    #
    design <- model.matrix(~ 0 + factor(c(rep(1, totalPairs), 
                rep(2, totalPairs))));
    colnames(design) <- c("LiverHCC", "Normal");
    rownames(design) <- colnames(miRNAseq);

    #   Data normalization
    #
    miRNAseq <- DGEList(counts=miRNAseq)
    miRNAseq <- calcNormFactors(miRNAseq)
    normalized.TCGA.LIHC.miRNA.Counts <- voom(miRNAseq, design)

    #   PCA plot
    #
    miRNAseq <- signif(normalized.TCGA.LIHC.miRNA.Counts$E)
    pca <- prcomp(miRNAseq);

    #   which(pca$rotation[,2]>0)[1:2]
    #   TCGA-BC-A10X-01A TCGA-BC-A110-01A 
    #              6                9
    
    group <- c(rep("LiverHCC", totalPairs), rep("Normal", totalPairs))
    ggplot(as.data.frame(pca$rotation), 
        aes(x=pca$rotation[,1], y=pca$rotation[,2])) + 
        geom_point(aes(color=group), size=4) + 
        labs(x="First PC", y="Second PC") +
        ggtitle("TCGA LIHC miRNASeqV2 Sample PCA Plot") +
        geom_text(label=colnames(miRNAseq)[6], size=3,
            aes(x=pca$rotation[6,1], y=pca$rotation[6,2]-0.01)) +
        geom_text(label=colnames(miRNAseq)[9], size=3,
            aes(x=pca$rotation[9,1]+0.003, y=pca$rotation[9,2]))



#   3.  Methylation data
#   ===============================================================

    load(paste0(filePath, "TCGAbiolinks.LIHC.Methylation.beta.Values.RData"))

    annotation <- lihc.methylation.data[, c(1:3)]
    methylation <- as.matrix(lihc.methylation.data[, -c(1:3)]);
    colnames(methylation) <- substr(colnames(methylation), 1, 16)

    noSymbols <- which(annotation$Gene_Symbol == "")
    methylation <- methylation[-noSymbols, ]

    keep <- complete.cases(methylation)
    methylation <- methylation[keep,]

    totalSamples <- ncol(methylation)
    totalPairs <- totalSamples/2

    #   sort the data first by sample name then by sample type
    #   to make sure samples are in the order of tumors followed
    #   by paired normal tissue.
    #
    methylation <- methylation[, order(colnames(methylation))]
    sampleOrder <- c(seq(1, totalSamples, by=2), seq(2, totalSamples, by=2))
    methylation <- methylation[, sampleOrder]

    #   Data normalization
    #
    methylation <- normalizeBetweenArrays(methylation, method="quantile")

    pca <- prcomp(methylation);

    #   which(pca$rotation[, 1] > 0.1375)[1:2]
    #   TCGA-BC-A10X-01A TCGA-BC-A110-01A 
    #              6                9 

    group <- c(rep("LiverHCC", totalPairs), rep("Normal", totalPairs))
    ggplot(as.data.frame(pca$rotation), 
        aes(x=pca$rotation[,1], y=pca$rotation[,2])) + 
        geom_point(aes(color=group), size=4) + 
        labs(x="First PC", y="Second PC") +
        ggtitle("TCGA LIHC Methylation Samples PCA Plot") +
        geom_text(label=colnames(miRNAseq)[6], size=3,
            aes(x=pca$rotation[6,1]-0.0025, y=pca$rotation[6,2])) +
        geom_text(label=colnames(miRNAseq)[9], size=3,
            aes(x=pca$rotation[9,1]-0.0025, y=pca$rotation[9,2]))


    #   Samples "TCGA-BC-A10X" and "TCGA-BC-A110" (index, 6, 9, 34, and 37)
    #   has to be removed later for differential expression analysis 


#   End of TCGA.LIHC.Data.QC.Plot.R
#   ======================================================================



