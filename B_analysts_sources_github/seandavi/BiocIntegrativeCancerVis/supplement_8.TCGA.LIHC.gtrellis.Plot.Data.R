#
#   Plot data for gtrelli plot
#
#   Items to be plotted: 
#
#   1.  Significantly differentially expressed genes (RNAseq) for
#       normal and tumor separately (two tracks of heatmap)
#   2.  Methylation of the genes above in tumor samples (lines)
#   3.  CNV of the genes above (scatters)
#   
#   Sample TCGA-BC-A10Q will be used since it has more CNVs.
#   ___________________________________________________________________________
#   <gtrellis plot><gtrellis plot><gtrellis plot><gtrellis plot><gtrellis plot>


    outPath <- "gtrellis.Plot/"
    dir.create(outPath)
    
    #   CNV data for scatter plot. Tumor and normal may have 
    #   different sites of cnv
    #   =========================================================

    load("TCGAbiolinks.Download/TCGAbiolinks.LIHC.CNV_SNP.seg_mean.RData")
    
    tumorID  <- "TCGA-BC-A10Q-01A"
    normalID <- "TCGA-BC-A10Q-11A"
    
    tumorRow <- grep(tumorID, lihc.CNV_SNP.data$Sample)
    tumorCNV <- lihc.CNV_SNP.data[tumorRow, -5]
    tumorCNV$Sample <- substr(tumorCNV$Sample, 1, 16)
    tumorCNV$Chromosome <- paste0("chr", tumorCNV$Chromosome)

    normalRow <- grep(normalID, lihc.CNV_SNP.data$Sample)
    normalCNV <- lihc.CNV_SNP.data[normalRow, -5]
    normalCNV$Sample <- substr(normalCNV$Sample, 1, 16)
    normalCNV$Chromosome <- paste0("chr", normalCNV$Chromosome)

    TCGA.LIHC.gtrellis.Plot.Tumor.CNV <- tumorCNV
    save(TCGA.LIHC.gtrellis.Plot.Tumor.CNV, 
        file=paste0(outPath, "TCGA.LIHC.gtrellis.Plot.Tumor.CNV.RData"))
    write.table(TCGA.LIHC.gtrellis.Plot.Tumor.CNV, sep="\t", 
        quote=FALSE, row.names=FALSE, col.names=TRUE,
        file=paste0(outPath, "TCGA.LIHC.gtrellis.Plot.Tumor.CNV.txt"))

    TCGA.LIHC.gtrellis.Plot.Normal.CNV <- normalCNV
    save(TCGA.LIHC.gtrellis.Plot.Normal.CNV, 
        file=paste0(outPath, "TCGA.LIHC.gtrellis.Plot.Normal.CNV.RData"))
    write.table(TCGA.LIHC.gtrellis.Plot.Normal.CNV, sep="\t", 
        quote=FALSE, row.names=FALSE, col.names=TRUE,
        file=paste0(outPath, "TCGA.LIHC.gtrellis.Plot.Normal.CNV.txt"))


    #   RNAseq data for heatmap plot
    #   =============================================================  

    load("TCGA.LIHC.Plot.Data/TCGA.LIHC.RNAseq.Plot.Data.RData")

    tumorID  <- "TCGA.BC.A10Q.01A"
    normalID <- "TCGA.BC.A10Q.11A"
    
    tumorCol <- grep(tumorID,colnames(TCGA.LIHC.RNAseq.Plot.Data))
    normalCol <- grep(normalID, colnames(TCGA.LIHC.RNAseq.Plot.Data))
    dataCol <- c(2, 4, 5, tumorCol,normalCol)
    TCGA.LIHC.gtrellis.Plot.RNAseq <- TCGA.LIHC.RNAseq.Plot.Data[, dataCol]

    save(TCGA.LIHC.gtrellis.Plot.RNAseq, 
        file=paste0(outPath, "TCGA.LIHC.gtrellis.Plot.data.RNAseq.RData"))
    write.table(TCGA.LIHC.gtrellis.Plot.RNAseq, sep="\t", quote=FALSE,
        row.names=FALSE, col.names=TRUE,
        file=paste0(outPath, "TCGA.LIHC.gtrellis.Plot.Data.RNAseq.txt"))


    #   Methylation data
    #   =============================================================

    load("TCGA.LIHC.Plot.Data/TCGA.LIHC.Methylation.Plot.Data.RData")

    tumorCol <- grep(tumorID,colnames(TCGA.LIHC.RNAseq.Plot.Data))
    normalCol <- grep(normalID, colnames(TCGA.LIHC.RNAseq.Plot.Data))
    dataCol <- c(2, 4, 5, tumorCol,normalCol)
    TCGA.LIHC.gtrellis.Plot.methylation <- TCGA.LIHC.Methylation.Plot.Data[, dataCol]

    save(TCGA.LIHC.gtrellis.Plot.methylation, 
        file=paste0(outPath, "TCGA.LIHC.gtrellis.Plot.Data.methylation.RData"))
    write.table(TCGA.LIHC.gtrellis.Plot.methylation, sep="\t", 
        quote=FALSE, row.names=FALSE, col.names=TRUE,
        file=paste0(outPath, "TCGA.LIHC.gtrellis.Plot.Data.methylation.txt"))


    #   miRNAseq data 
    #   =============================================================

    load("TCGA.LIHC.Plot.Data/TCGA.LIHC.miRNAseq.Plot.Data.RData")
    
    tumorCol <- grep(tumorID,colnames(TCGA.LIHC.miRNAseq.Plot.Data))
    normalCol <- grep(normalID, colnames(TCGA.LIHC.miRNAseq.Plot.Data))
    dataCol <- c(2, 4, 5, tumorCol, normalCol)
    TCGA.LIHC.gtrellis.Plot.miRNAseq <- TCGA.LIHC.miRNAseq.Plot.Data[, dataCol]

    save(TCGA.LIHC.gtrellis.Plot.miRNAseq, 
        file=paste0(outPath, "TCGA.LIHC.gtrellis.Plot.miRNAseq.RData"))
    write.table(TCGA.LIHC.gtrellis.Plot.miRNAseq, sep="\t", 
        quote=FALSE, row.names=FALSE, col.names=TRUE,
        file=paste0(outPath, "TCGA.LIHC.gtrellis.Plot.miRNAseq.txt"))


#   End of TCGA.LIHC.gtrellis.Plot.Data.RData
#   ======================================================================




































































































