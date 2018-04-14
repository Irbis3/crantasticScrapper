#
#   Plot data for complex heatmap
#
#   1.  A list of heatmap of RNAseq and miRNAseq reads which are 
#       ignificantly defferentially expressed between tumor and 
#       normal tissues.
#   2.  Annotated heatmap with tissue group, age group and diagnosis
#   3.  OncoPrint for mutations and SNVs
#   ____________________________________________________________________
#   <complex heatmap><complex heatmap><complex heatmap><complex heatmap>


    outPath <- "ComplexHeatmap.Plot/"
    dir.create(outPath)

    #   RNAseq data: total 114 genes for heatmap. No genome coordinate
    #   information needed
    #   ==============================================================

    load("TCGA.LIHC.Plot.Data/TCGA.LIHC.RNAseq.Plot.Data.RData")
    load("TCGA.LIHC.Data.Processed/TCGA.LIHC.RNAseqV2.lmfit.result.Rdata")
    
    absFC <- abs(TCGA.LIHC.RNAseqV2.lmfit.result$logFC)
    rows  <- which(absFC>=4)
    geneID <- rownames(TCGA.LIHC.RNAseqV2.lmfit.result)[rows]
    geneNames <- gsub("\\|.{1,}", "", geneID)

    rows <- which(rownames(TCGA.LIHC.RNAseq.Plot.Data) %in% geneNames)
    ComplexHeatmap.Data <- TCGA.LIHC.RNAseq.Plot.Data[rows,]
    TCGA.LIHC.RNAseq.ComplexHeatmap.Data <- ComplexHeatmap.Data[,-c(1:5)]

    save(TCGA.LIHC.RNAseq.ComplexHeatmap.Data, 
        file=paste0(outPath, "TCGA.LIHC.RNAseq.ComplexHeatmap.Data.RData"))
    write.table(TCGA.LIHC.RNAseq.ComplexHeatmap.Data, sep="\t",
            quote=FALSE, row.names=TRUE, col.names=TRUE,
            file=paste0(outPath, "TCGA.LIHC.RNAseq.ComplexHeatmap.Data.txt"))

    geneNames <- rownames(TCGA.LIHC.RNAseq.ComplexHeatmap.Data)



    #   miRNAseq data: top 114 miRNA for heatmap
    #   ===============================================================

    load("TCGA.LIHC.Data.Processed/TCGA.LIHC.miRNAseqV2.lmfit.result.Rdata")
    load("TCGA.LIHC.Plot.Data/TCGA.LIHC.miRNAseq.Plot.Data.RData")
   
    mirnaName <- rownames(TCGA.LIHC.miRNAseq.Plot.Data)
    lmfitName <- rownames(TCGA.LIHC.miRNAseqV2.lmfit.result)
    lmfitRows <- which( lmfitName %in% mirnaName)
    lmfitData <- TCGA.LIHC.miRNAseqV2.lmfit.result[lmfitRows,]
    
    absFC <- abs(lmfitData$logFC)
    fcOrder  <- order(absFC, decreasing=TRUE)
    lmfitData <- lmfitData[fcOrder,]
    mirnaID <- rownames(lmfitData)[1:114]
    
    rows <- which(rownames(TCGA.LIHC.miRNAseq.Plot.Data) %in% mirnaID)
    miRNAseq.Data <- TCGA.LIHC.miRNAseq.Plot.Data[rows,]
    TCGA.LIHC.miRNAseq.ComplexHeatmap.Data <- miRNAseq.Data[, -c(1:5)]
    
    save(TCGA.LIHC.miRNAseq.ComplexHeatmap.Data, 
        file=paste0(outPath,"TCGA.LIHC.miRNAseq.ComplexHeatmap.Data.RData"))
    write.table(TCGA.LIHC.miRNAseq.ComplexHeatmap.Data, sep="\t",
            quote=FALSE, row.names=TRUE, col.names=TRUE,
            file=paste0(outPath,"TCGA.LIHC.miRNAseq.ComplexHeatmap.Data.txt"))


    #   OncoPrint data: data frame with columns for genes and rows for
    #   samplesl and values are "  ", "MUT;", "MUT;AMP;", "AMP;",     
    #   "HOMDEL;",  NA
    #   ================================================================

    load("TCGAbiolinks.Download/TCGAbiolinks.LIHC.Mutation.Data.RData")

    tumors <- grep("01A$", colnames(TCGA.LIHC.RNAseq.ComplexHeatmap.Data))
    tumorID <- colnames(TCGA.LIHC.RNAseq.ComplexHeatmap.Data)[tumors]
    tumorID <- gsub("\\.", "-", tumorID)
    
    samples <- substr(lihc.Mutation.data$Tumor_Sample_Barcode, 1, 16)
    mutationData <- lihc.Mutation.data[which(samples %in% tumorID),]
    samples  <- substr(mutationData$Tumor_Sample_Barcode, 1, 16)
    
    geneList  <- unique(as.character(mutationData$Hugo_Symbol))    
    TCGA.LIHC.Mutation.Table <- geneList;
    
    for(aSam in seq_along(tumorID))
    {
        mutatType <- rep("  ", length(geneList))
        rows <- grep(tumorID[aSam], samples)
        mutation <- mutationData[rows, c(1, 9, 10)]
        
        for(aMut in seq_len(nrow(mutation)))
        {
            gene <- as.character(mutation[aMut, 1])
            mut <- as.character(mutation[aMut,2])
            if(mut == "RNA") 
                mut <- paste0(mut, ":", as.character(mutation[aMut,3]))

            index <- which(geneList == gene)
            mutatType[index] <- paste0(mut, ";")
        }
        TCGA.LIHC.Mutation.Table <- rbind(TCGA.LIHC.Mutation.Table, mutatType)
    }

    colnames(TCGA.LIHC.Mutation.Table) <- geneList;
    TCGA.LIHC.Mutation.Table <- TCGA.LIHC.Mutation.Table[-1,]
    rownames(TCGA.LIHC.Mutation.Table) <- tumorID
    
    save(TCGA.LIHC.Mutation.Table, 
        file=paste0(outPath,"TCGA.LIHC.Mutation.Table.RData"))
    write.table(TCGA.LIHC.Mutation.Table, sep="\t", quote=FALSE,
        row.names=TRUE, col.names=TRUE, 
        file=paste0(outPath,"TCGA.LIHC.Mutation.Table.txt"))

    #   keep genes with more mutation samples
    #
    noMut <- rep(0, length(geneList))
    for(aGene in seq_along(geneList))
    {
        count <- which(TCGA.LIHC.Mutation.Table[,aGene] == "  ")
        noMut[aGene] <- length(count)   
    }

    min(noMut)  #   [1] 15
    max(noMut)  #   [1] 25
    columns <- which(noMut<23)
    TCGA.LIHC.OncoPrint.Data <- TCGA.LIHC.Mutation.Table[, columns]

    save(TCGA.LIHC.OncoPrint.Data, 
        file=paste0(outPath,"TCGA.LIHC.OncoPrint.Data.RData"))
    write.table(TCGA.LIHC.OncoPrint.Data, sep="\t", quote=FALSE,
        row.names=TRUE, col.names=TRUE, 
        file=paste0(outPath,"TCGA.LIHC.OncoPrint.Data.txt"))


    #   Clinical data
    #   =============================================================
    #
    load("TCGAbiolinks.Download/TCGA.LIHC.Clinical.Data.RData")

    sampleID <- colnames(TCGA.LIHC.RNAseq.ComplexHeatmap.Data)
    sampleID <- gsub("\\.", "-", sampleID)

    rows <- which(lihc.clinical$bcr_sample_barcode %in% sampleID)
    clinical <- lihc.clinical[rows, c(3,8,11,21,22)]
    rownames(clinical) <- clinical[,1]
    
    TCGA.LIHC.Clinical.Data <- clinical;
    save(TCGA.LIHC.Clinical.Data, 
        file=paste0(outPath,"TCGA.LIHC.Clinical.Data.RData"))
    write.table(TCGA.LIHC.Clinical.Data,  sep="\t", 
        quote=FALSE, row.name=FALSE, col.names=TRUE,
        file=paste0(outPath,"TCGA.LIHC.Clinical.Data.txt"))
)


    
#   End of TCGA.LIHC.ComplexHeatmap.Plot.Data.RData
#   =====================================================================    
    
    