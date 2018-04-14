#
#   caOmicsV plot data
#
#   1.  Genes significant differentially expressed between tumor 
#       and normal tissues
#   2.  miRNA most negatively correlated to the genes above
#   3.  Methylation of genes above
#   4.  SNV of genes above
#   5.  Mutations of genes above
#
#   All data are sample based and do not need genome axis data
#   ___________________________________________________________________________
#   <caOmicsV plot><caOmicsV plot><caOmicsV plot><caOmicsV plot><caOmicsV plot>


    outPath <- "caOmicsV.Plot/"
    dir.create(outPath)

    #   1.  List of genes most positively correlated to the gene with  
    #       highest positive fold change from differential analysis 
    #       and have significant negatively correlated miRNA
    #   =====================================================================
    #
    load("TCGA.LIHC.Data.Processed/TCGA.LIHC.RNAseqV2.lmfit.result.Rdata")
    lmRes <- TCGA.LIHC.RNAseqV2.lmfit.result
    lmRes <- lmRes[order(lmRes$logFC), ]
    highestFCgene <- sub("\\|.{1,}", "", rownames(lmRes)[nrow(lmRes)])
    
    #   correlation of all genes to genes with highest fold change
    #
    load("TCGA.LIHC.Plot.Data/TCGA.LIHC.RNAseq.Plot.Data.RData")

    highFCrow <- which(rownames(TCGA.LIHC.RNAseq.Plot.Data) == highestFCgene)
    highFCexpr <- as.numeric(TCGA.LIHC.RNAseq.Plot.Data[highFCrow, -c(1:5)])

    expr <- as.matrix(TCGA.LIHC.RNAseq.Plot.Data[,-c(1:5)])
    corrValues <- cor(t(expr), highFCexpr)
    absCorr <- data.frame(corrValues, absCorrValues=abs(corrValues))
    absCorr <- absCorr[order(absCorr$absCorrValues, decreasing=TRUE), ]

    #   Filter the gene correlation by RNA/miRNA correlation
    #
    load("TCGA.LIHC.Plot.Data/TCGA.LIHC.miRNA2RNA.Plot.Data.RData")

    miRNAgene <- gsub("\\|.{1,}", "", rownames(TCGA.LIHC.miRNA2RNA.Plot.Data))
    absCorr <- absCorr[which(rownames(absCorr) %in% miRNAgene),]
    
    geneList <- rownames(absCorr)[which(absCorr$absCorrValues>0.7)]
    geneList <- geneList[order(geneList)]
    
    #    geneList
    #   [1] "AADAT"    "ACAD11"   "CFL2"     "DMD"      "FERMT2"   "GATM"    
    #   [7] "IQGAP3"   "KIAA1522" "MUC13"    "NDRG2"    "PAQR4"    "PGM1"    
    #   [13] "RANBP3L"  "SLC16A2"  "TMEM56"   "TPPP2"    "TTC39A" 


    #   2.  RNAseq data for genes in the list of above
    #   ==============================================================
    #
    geneRow <- which(rownames(TCGA.LIHC.RNAseq.Plot.Data) %in% geneList)
    RNAseq <- TCGA.LIHC.RNAseq.Plot.Data[geneRow, -c(2:5)]
    colnames(RNAseq) <- gsub("01A$", "LiverHCC", colnames(RNAseq))
    colnames(RNAseq) <- gsub("11A$", "Normal", colnames(RNAseq))

    head(RNAseq)[,1:3]
    #          hgnc_symbol TCGA.BC.A10Q.LiverHCC TCGA.BC.A10R.LiverHCC
    #   AADAT        AADAT           -0.01366579             -2.462784
    #   ACAD11      ACAD11            4.34556084              5.140036
    #   CFL2          CFL2            3.03340246              4.493939
    #   DMD            DMD            2.17683082              5.012597
    #   FERMT2      FERMT2            5.26011395              5.387335
    #   GATM          GATM            7.06256810              9.041452
    #
    #   class(RNAseq)                   #   [1] "data.frame"
    #   length(geneList)                #   [1] 17
    #   dim(RNAseq)                     #   [1] 17 53
    #   sum(rownames(RNAseq)==geneList) #   [1] 17

    TCGA.LIHC.RNAseq.caOmicsV.Plot.Data <- RNAseq[order(rownames(RNAseq)),]
    save(TCGA.LIHC.RNAseq.caOmicsV.Plot.Data, 
        file=paste0(outPath, "TCGA.LIHC.RNAseq.caOmicsV.Plot.Data.RData"))
    write.table(TCGA.LIHC.RNAseq.caOmicsV.Plot.Data, sep="\t",
        quote=FALSE, row.names=FALSE, col.names=TRUE,
        file=paste0(outPath, "TCGA.LIHC.RNAseq.caOmicsV.Plot.Data.txt"))


    #   3.  miRNA data. Each row is miRNA expr values for RNA/miRNA pair 
    #       that is most negativelly correlated and some miRNAs rows may
    #       be same. Also the genes should be one from gene list above and
    #       row headers will be gene names other than miRNA ID.
    #   ==============================================================
    #
    miRNAgene <- gsub("\\|.{1,}", "", rownames(TCGA.LIHC.miRNA2RNA.Plot.Data))
    mirRow  <- which(miRNAgene %in% geneList)
    miRNAseq <- TCGA.LIHC.miRNA2RNA.Plot.Data[mirRow, -c(1:5)]

    miRNAgene <- gsub("\\|.{1,}", "", rownames(miRNAseq))
    miRNAseq <- data.frame(geneSymbol=miRNAgene, miRNAseq)

    colnames(miRNAseq) <- gsub("01A$", "LiverHCC", colnames(miRNAseq))
    colnames(miRNAseq) <- gsub("11A$", "Normal", colnames(miRNAseq))
    miRNAseq <- miRNAseq[order(miRNAseq[,1]),]

    #   head(miRNAseq)[,1:3]
    #                       geneSymbol TCGA.BC.A10Q.LiverHCC TCGA.BC.A10R.LiverHCC
    #   AADAT|hsa-mir-181c       AADAT           6.274907           4.778152
    #   ACAD11|hsa-mir-212      ACAD11           3.502528           2.238730
    #   CFL2|hsa-mir-181b-1       CFL2           8.729618           9.443715
    #   DMD|hsa-mir-181d           DMD           4.303699           2.831820
    #   FERMT2|hsa-mir-21       FERMT2          19.405776          17.444480
    #   GATM|hsa-mir-132          GATM           7.922552           6.030279
    #
    #   class(miRNAseq)                 #   [1] "data.frame"
    #   dim(miRNAseq)                   #   [1] 17 53
    #   sum(miRNAgene == geneList)      #   [1] 17

    TCGA.LIHC.miRNA.caOmicsV.Plot.Data <- miRNAseq
    save(TCGA.LIHC.miRNA.caOmicsV.Plot.Data,
            file=paste0(outPath, "TCGA.LIHC.miRNA.caOmicsV.Plot.Data.RData"))
    write.table(TCGA.LIHC.miRNA.caOmicsV.Plot.Data, sep="\t", 
            quote=FALSE, col.names=TRUE, row.names=FALSE,
            file=paste0(outPath, "TCGA.LIHC.miRNA.caOmicsV.Plot.Data.txt"))


    #   Methylation data for the same genes above
    #   ==============================================================
    #
    load("TCGA.LIHC.Plot.Data/TCGA.LIHC.Methylation.Plot.Data.RData")

    methyRow <- which(rownames(TCGA.LIHC.Methylation.Plot.Data) %in% geneList)
    methylation <- TCGA.LIHC.Methylation.Plot.Data[methyRow, -c(2:5)]
    methylation <- methylation[order(rownames(methylation)), ]
    colnames(methylation) <- gsub("01A$", "LiverHCC", colnames(methylation))
    colnames(methylation) <- gsub("11A$", "Normal", colnames(methylation))

    #   class(methylation)                    #   [1] "data.frame"
    #   dim(methylation)                      #   [1] 17 53
    #   sum(rownames(methylation)==geneList)  #   [1] 17

    #   This is for bar/histogram plot of bioNetwork layout
    #
    TCGA.LIHC.Methylation.caOmicsV.Plot.Data <- methylation
    save(TCGA.LIHC.Methylation.caOmicsV.Plot.Data, 
        file=paste0(outPath, "TCGA.LIHC.Methylation.caOmicsV.Plot.Data.RData"))
    write.table(TCGA.LIHC.Methylation.caOmicsV.Plot.Data, sep="\t",
        quote=FALSE, col.names=TRUE, row.names=FALSE,
        file=paste0(outPath, "TCGA.LIHC.Methylation.caOmicsV.Plot.Data.txt"))

    #   This is for category data plot of bioMetriz laout
    #
    totalSamples <- ncol(methylation) - 1
    methyCatogery <- matrix(rep(0, totalSamples*nrow(methylation)), 
                        ncol=totalSamples)
    for(aGene in seq_along(geneList))
    {
        methylValue <- as.numeric(methylation[aGene, -1])
        methyCatogery[aGene, which(methylValue>0.5)] <- 1
    }
    rownames(methyCatogery) <- rownames(methylation)   
    colnames(methyCatogery) <- colnames(methylation)[-1]
    methyCatogery <- data.frame(geneSymbol=rownames(methylation), methyCatogery)

    #   head(methyCatogery)[,1:3]
    #          geneSymbol TCGA.BC.A10Q.LiverHCC TCGA.BC.A10R.LiverHCC
    #   AADAT       AADAT                  0                  0
    #   ACAD11     ACAD11                  0                  0
    #   CFL2         CFL2                  0                  0
    #   DMD           DMD                  0                  0
    #   FERMT2     FERMT2                  0                  0
    #   GATM         GATM                  0                  0
    #
    #   class(methyCatogery)                    #   [1] "data.frame"
    #   dim(methyCatogery)                      #   [1] 17 53
    #   sum(rownames(methyCatogery)==geneList)  #   [1] 17

    TCGA.LIHC.methyCatogery.caOmicsV.Plot.Data <- methyCatogery
    save(TCGA.LIHC.methyCatogery.caOmicsV.Plot.Data,
        file=paste0(outPath,"TCGA.LIHC.methyCatogery.caOmicsV.Plot.Data.RData"))
    write.table(TCGA.LIHC.methyCatogery.caOmicsV.Plot.Data, 
        sep="\t", quote=FALSE, col.names=TRUE, row.names=FALSE,
        file=paste0(outPath, "TCGA.LIHC.methyCatogery.caOmicsV.Plot.Data.txt"))



    #   CNV data
    #   ==============================================================
    load("TCGA.LIHC.Plot.Data/TCGA.LIHC.CNV.Plot.Data.RData")

    cnvRow <- which(rownames(TCGA.LIHC.CNV.Plot.Data) %in% geneList)
    cnvData <- TCGA.LIHC.CNV.Plot.Data[cnvRow, -c(2:5)]

    cnvData <- cnvData[order(rownames(cnvData)), ] 
    colnames(cnvData) <- gsub("01A$", "LiverHCC", colnames(cnvData))
    colnames(cnvData) <- gsub("11A$", "Normal", colnames(cnvData))

    #   head(cnvData)[,1:3]
    #          hgnc_symbol TCGA.BC.A10Q.LiverHCC TCGA.BC.A10R.LiverHCC
    #   AADAT        AADAT           0.0279       -0.0031000
    #   ACAD11      ACAD11           0.0241        0.2475000
    #   CFL2          CFL2          -0.2899        0.2570000
    #   DMD            DMD           0.0152       -0.1973333
    #   FERMT2      FERMT2          -0.2984       -0.3540000
    #   GATM          GATM           0.2677       -0.0036000
    #
    #   class(cnvData)                      #   [1] "data.frame"
    #   dim(cnvData)                        #   [1] 17 53
    #   sum(rownames(cnvData) == geneList)  #   [1] 17

    TCGA.LIHC.CNV.caOmicsV.Plot.Data <- cnvData
    save(TCGA.LIHC.CNV.caOmicsV.Plot.Data, 
        file=paste0(outPath, "TCGA.LIHC.CNV.caOmicsV.Plot.Data.RData"))
    write.table(TCGA.LIHC.CNV.caOmicsV.Plot.Data,
        sep="\t", quote=FALSE, row.names=FALSE, col.names=TRUE,
        file=paste0(outPath, "TCGA.LIHC.CNV.caOmicsV.Plot.Data.txt"))



    #   Mutation data
    #   ==============================================================
    #
    load("TCGA.LIHC.Plot.Data/TCGA.LIHC.Mutation.Plot.Data.RData")

    mutRow <- which(rownames(TCGA.LIHC.Mutation.Plot.Data) %in% geneList)
    mutation <- TCGA.LIHC.Mutation.Plot.Data[mutRow, -c(2:5)]
    mutation <- mutation[order(rownames(mutation)),]
    colnames(mutation) <- gsub("01A$", "LiverHCC", colnames(mutation))
    colnames(mutation) <- gsub("11A$", "Normal", colnames(mutation))

    #   head(mutation)[,1:3]
    #          hgnc_symbol TCGA.BC.A10Q.LiverHCC TCGA.BC.A10R.LiverHCC
    #   AADAT        AADAT                  0                  0
    #   ACAD11      ACAD11                  0                  0
    #   CFL2          CFL2                  0                  0
    #   DMD            DMD                  0                  0
    #   FERMT2      FERMT2                  0                  0
    #   GATM          GATM                  0                  0
    #
    #   class(mutation)                     #   [1] "data.frame"
    #   dim(mutation)                       #   [1] 17 53
    #   sum(rownames(mutation) == geneList) #   [1] 20

    TCGA.LIHC.Mutation.caOmicsV.Plot.Data <- mutation
    save(TCGA.LIHC.Mutation.caOmicsV.Plot.Data,
        file=paste0(outPath, "TCGA.LIHC.Mutation.caOmicsV.Plot.Data.RData"))
    write.table(TCGA.LIHC.Mutation.caOmicsV.Plot.Data,
        sep="\t", quote=FALSE, row.names=FALSE, col.names=TRUE,
        file=paste0(outPath, "TCGA.LIHC.Mutation.caOmicsV.Plot.Data.txt"))



    #   RNA and miRNA link table
    #   ==============================================================

    RNA2miRNA <- data.frame(
        geneSymbol=gsub("\\|.{1,}", "", rownames(miRNAseq)),
        miRNA_ID=gsub(".{1,}\\|", "", rownames(miRNAseq)) )

    TCGA.LIHC.RNA2miRNA.Link.caOmicsV.Plot.Data <- RNA2miRNA
    save(TCGA.LIHC.RNA2miRNA.Link.caOmicsV.Plot.Data,
      file=paste0(outPath, "TCGA.LIHC.RNA2miRNA.Link.caOmicsV.Plot.Data.RData"))
    write.table(TCGA.LIHC.RNA2miRNA.Link.caOmicsV.Plot.Data,
        sep="\t", quote=FALSE, row.names=FALSE, col.names=TRUE,
        file=paste0(outPath, "TCGA.LIHC.RNA2miRNA.Link.caOmicsV.Plot.Data.txt"))



    #   Sample information
    #   ==============================================================

    sampleID <- colnames(RNAseq)[-1]
    sample_type <- c(rep("LiverHCC", length(sampleID)/2), 
                 rep("Normal", length(sampleID)/2))
                           
    sampleInfo <- data.frame(sampleID, sample_type)

    #   head(sampleInfo)
    #                  sampleID sample_type
    #   1 TCGA.BC.A10Q.LiverHCC    LiverHCC
    #   2 TCGA.BC.A10R.LiverHCC    LiverHCC
    #   3 TCGA.BC.A10T.LiverHCC    LiverHCC
    #   4 TCGA.BC.A10U.LiverHCC    LiverHCC
    #   5 TCGA.BC.A10W.LiverHCC    LiverHCC
    #   6 TCGA.BC.A10Y.LiverHCC    LiverHCC
    #
    #   class(sampleInfo)   #   "data.frame"
    #   dim(sampleInfo)     #   [1]  52 5

    TCGA.LIHC.SampleInfo.caOmicsV.Plot.Data <- sampleInfo
    save(TCGA.LIHC.SampleInfo.caOmicsV.Plot.Data,
        file=paste0(outPath, "TCGA.LIHC.SampleInfo.caOmicsV.Plot.Data.RData"))
    write.table(TCGA.LIHC.SampleInfo.caOmicsV.Plot.Data, 
        sep="\t", quote=FALSE, row.names=FALSE, col.names=TRUE,
        file=paste0(outPath, "TCGA.LIHC.SampleInfo.caOmicsV.Plot.Data.txt"))



    #   Summary data:  correlation coefficient values and log fold
    #   change between tumor and normal
    #   ==============================================================
    #

    #   correlation between miRNA and RNA in tumor samples
    #
    load("TCGA.LIHC.Plot.Data/TCGA.LIHC.RNAseq_miRNAseq_corr_by_group.RData")

    corrTable <- TCGA.LIHC.RNAseq_miRNAseq_corr
    corrGene <- gsub("\\|.{1,}", "", corrTable$geneSymbol)
    corrRow  <- which(corrGene %in% geneList)
    RNA2MIR_corr <- TCGA.LIHC.RNAseq_miRNAseq_corr[corrRow,c(1,3)]
    rownames(RNA2MIR_corr) <- paste(corrTable[corrRow,1], 
            corrTable[corrRow,2], sep="|")
    RNA2MIR_corr <- RNA2MIR_corr[order(RNA2MIR_corr[,1]),]

    #   head(RNA2MIR_corr)
    #                       geneSymbol tumor.corr.values
    #   AADAT|hsa-mir-181c       AADAT        -0.7257787
    #   ACAD11|hsa-mir-212      ACAD11        -0.7304093
    #   CFL2|hsa-mir-181b-1       CFL2        -0.6609745
    #   DMD|hsa-mir-181d           DMD        -0.6964373
    #   FERMT2|hsa-mir-21       FERMT2        -0.5475020
    #   GATM|hsa-mir-132          GATM        -0.7242463
    #
    #   class(RNA2MIR_corr)                       #   [1] "data.frame"
    #   dim(RNA2MIR_corr)                         #   [1] 17  2
    #   sum(RNA2MIR_corr$geneSymbol == geneList)  #   [1] 17

    TCGA.LIHC.RNA2MIR.Corr.caOmicsV.Plot.Data <- RNA2MIR_corr
    save(TCGA.LIHC.RNA2MIR.Corr.caOmicsV.Plot.Data,
        file=paste0(outPath, "TCGA.LIHC.RNA2MIR.Corr.caOmicsV.Plot.Data.RData"))
    write.table(TCGA.LIHC.RNA2MIR.Corr.caOmicsV.Plot.Data,
        sep="\t", quote=FALSE, col.names=TRUE, row.names=TRUE,
        file=paste0(outPath, "TCGA.LIHC.RNA2MIR.Corr.caOmicsV.Plot.Data.txt"))


    #   logFC between tumor and normal tissues
    #
    tumor_mean <- rowMeans(as.matrix(RNAseq[, 2:27]))
    normal_mean <- rowMeans(as.matrix(RNAseq[, 28:53]))
    logFC <- tumor_mean - normal_mean
    T2N_logFC <- data.frame(geneSymbol=RNAseq$hgnc_symbol, logFC)

    #   head(T2N_logFC)
    #            geneSymbol     logFC
    #   AADAT         AADAT -3.822238
    #   ACAD11       ACAD11 -1.639057
    #   CFL2           CFL2 -1.026529
    #   DMD             DMD -1.641714
    #   FERMT2       FERMT2 -1.105615
    #   GATM           GATM -1.338018
    #
    #   class(T2N_logFC)                        #   [1] "data.frame"
    #   dim(T2N_logFC)                          #   [1] 17  2
    #   sum(rownames(T2N_logFC) == geneList)    #   [1] 17

    TCGA.LIHC.TN_logFC.caOmicsV.Plot.Data <- T2N_logFC
    save(TCGA.LIHC.TN_logFC.caOmicsV.Plot.Data,
        file=paste0(outPath, "TCGA.LIHC.TN_logFC.caOmicsV.Plot.Data.RData"))
    write.table(TCGA.LIHC.TN_logFC.caOmicsV.Plot.Data,
        sep="\t", quote=FALSE, row.names=FALSE, col.names=TRUE,
        file=paste0(outPath, "TCGA.LIHC.TN_logFC.caOmicsV.Plot.Data.txt"))


#   End of TCGA.LIHC.caOmicsV.Plot.Data.RANBP3L
#   ==========================================================================















