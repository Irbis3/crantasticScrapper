#
#   TCGA LIHC data for CIRCOS plot with RCircos package
#
#   We will plot two circular heatmap with RNAseq reads (gene) and 
#   miRNAseq reads, DNA methylation of the genes and links of few miRNAs 
#   to their most negatively correlated genes
#
#   Reference:
#
#   Hongen Zhang, Paul Meltzer, Sean Davis.RCircos: an R package for 
#   Circos 2D track plots.BMC Bioinformatics 2013; 
#   DOI 10.1186/1471-2105-14-244
#   https://cran.r-project.org/web/packages/RCircos/index.html
#
#   Last modified on May 12, 2016
#   ______________________________________________________________________
#   <RCircos plot><RCircos plot><RCircos plot><RCircos plot><RCircos plot>



    outPath <- "RCicos.Plot/"
    dir.create(outPath)

    #   1.  Genes to be plotted. To have better resolution, reduce total
    #       genes to be plotted by choosing the genes with abs FC >= 3
    #   ================================================================
    #
    load("TCGA.LIHC.Data.Processed/TCGA.LIHC.RNAseqV2.lmfit.result.Rdata")
    load("TCGA.LIHC.Plot.Data/TCGA.LIHC.RNAseq_miRNAseq_corr_by_group.RData")
    
    absFC <- abs(TCGA.LIHC.RNAseqV2.lmfit.result$logFC)
    highFC <- which(absFC >= 3)
    geneNames <- rownames(TCGA.LIHC.RNAseqV2.lmfit.result)[highFC]
    geneNames <- gsub("\\|.{1,}", "", geneNames)
    
    corrRow <- which(TCGA.LIHC.RNAseq_miRNAseq_corr$geneSymbol %in% geneNames)
    geneID <- as.character(TCGA.LIHC.RNAseq_miRNAseq_corr$geneSymbol[corrRow])
    mirnaID <- as.character(TCGA.LIHC.RNAseq_miRNAseq_corr$miRNA_ID[corrRow])
    mirnaID <- unique(mirnaID)
    
    length(geneID)      #   [1] 279
    length(mirnaID)     #   [1] 130    


    #   2.  RNAseq plot data for heatmap
    #   ===================================================================
    load("TCGA.LIHC.Plot.Data/TCGA.LIHC.RNAseq.Plot.Data.RData")
    rnaRow <- which(rownames(TCGA.LIHC.RNAseq.Plot.Data) %in% geneID)
    
    rnaAnnot <- TCGA.LIHC.RNAseq.Plot.Data[rnaRow, c(2,4,5,1)]
    rnaExpr  <- TCGA.LIHC.RNAseq.Plot.Data[rnaRow, -c(1:5)]
    TCGA.LIHC.RNAseq.RCircos.Plot.Data <- data.frame(rnaAnnot, rnaExpr)
    colnames(TCGA.LIHC.RNAseq.RCircos.Plot.Data)[1:4] <- c("Chromosome",  
                "ChromStart","ChromEnd", "GeneName")

    save(TCGA.LIHC.RNAseq.RCircos.Plot.Data, 
        file=paste0(outPath,"TCGA.LIHC.RNAseq.RCircos.Plot.Data.RData"))
    write.table(TCGA.LIHC.RNAseq.RCircos.Plot.Data, sep="\t", 
        quote=FALSE, row.names=FALSE, col.names=TRUE, 
        file=paste0(outPath,"TCGA.LIHC.RNAseq.RCircos.Plot.Data.txt"))


    #   3.  miRNA plot data for heatmap
    #   =============================================================
    load("TCGA.LIHC.Plot.Data/TCGA.LIHC.miRNAseq.Plot.Data.RData")
    mirRow <- which(rownames(TCGA.LIHC.miRNAseq.Plot.Data) %in% mirnaID)

    mirAnnot <- TCGA.LIHC.miRNAseq.Plot.Data[mirRow, c(2,4,5,1)]
    mirExpr  <- TCGA.LIHC.miRNAseq.Plot.Data[mirRow, -c(1:5)]
    
    TCGA.LIHC.miRNAseq.RCircos.Plot.Data <- data.frame(mirAnnot, mirExpr)
    colnames(TCGA.LIHC.miRNAseq.RCircos.Plot.Data)[1:4] <- c("Chromosome",  
                "ChromStart","ChromEnd", "GeneName")

    save(TCGA.LIHC.miRNAseq.RCircos.Plot.Data, 
        file=paste0(outPath,"TCGA.LIHC.miRNAseq.RCircos.Plot.Data.RData"))
    write.table(TCGA.LIHC.miRNAseq.RCircos.Plot.Data, sep="\t", 
        quote=FALSE, row.names=FALSE, col.names=TRUE, 
        file=paste0(outPath,"TCGA.LIHC.miRNAseq.RCircos.Plot.Data.txt"))


    #   4.  RNA and miRAN genomic coordinates for link lines.
    #   ===============================================================

    #   select RNA/miRNA pairs that have both expre values
    #
    miRNA2RNA <- TCGA.LIHC.RNAseq_miRNAseq_corr[corrRow, 1:3]
    miRNA2RNA <- miRNA2RNA[which(miRNA2RNA[,1] %in% rownames(rnaExpr)),]
    miRNA2RNA <- miRNA2RNA[which(miRNA2RNA[,2] %in% rownames(mirExpr)),]
    miRNA2RNA <- miRNA2RNA[order(miRNA2RNA[,3]), ]
    miRNA2RNA <- miRNA2RNA[which(miRNA2RNA[,3] <= -0.65), ]
    
    #   select top miRNA which have 3 or more correlated RNA(gene)s
    #
    mirnaID <- unique(miRNA2RNA[,2])
    numRNA  <- rep(0, length(mirnaID))
    for(aMir in seq_along(mirnaID))
        numRNA[aMir] <- length(which(miRNA2RNA[,2] == mirnaID[aMir]))
    mirnaID <- mirnaID[which(numRNA>=3)]
    numRNA  <- numRNA[which(numRNA>=3)]
    miRNA2RNA <- miRNA2RNA[which(miRNA2RNA[,2] %in% mirnaID), ]

    #   Pair genomic coordinates for RNA and miRNA pairs
    #
    mirAxis <- mirAnnot[which(mirAnnot[,4] %in% mirnaID), ]
    mirAxis <- mirAxis[order(rownames(mirAxis)), ]
    numRNA  <- numRNA[order(mirnaID)]
    for(aNum in seq_along(numRNA))
    {
        for(aMir in 1:(numRNA[aNum]-1))
            mirAxis <- rbind(mirAxis, mirAxis[aNum,])
    }
    mirAxis <- mirAxis[order(mirAxis[,4]), ]

    geneAxis <- rnaAnnot[which(rnaAnnot[,4] %in% miRNA2RNA[,1]),]
    geneAxis <- geneAxis[order(rownames(geneAxis)),]
    
    miRNA2RNA <- miRNA2RNA[order(miRNA2RNA[,1]), ]
    sum(rownames(geneAxis) == as.character(miRNA2RNA[,1]))  #   [1] 14

    TCGA.LIHC.RNA_miRNA.link <- cbind(miRNA2RNA[,1:2], geneAxis);
    mirOrder <- order(TCGA.LIHC.RNA_miRNA.link[,2])
    TCGA.LIHC.RNA_miRNA.link <- TCGA.LIHC.RNA_miRNA.link[mirOrder,]
    sum(TCGA.LIHC.RNA_miRNA.link[,2] == mirAxis[, 4])   #   [1] 14

    TCGA.LIHC.RNA_miRNA.link <- data.frame(TCGA.LIHC.RNA_miRNA.link, mirAxis)
    TCGA.LIHC.RNA_miRNA.link <- TCGA.LIHC.RNA_miRNA.link[, c(1, 3:5, 2,7:9)]
    colnames(TCGA.LIHC.RNA_miRNA.link) <- c("Gene","Chromosome", "chromStart", 
        "chromEnd", "miRNA", "Chromosome", "chromStart", "chromEnd")

    save(TCGA.LIHC.RNA_miRNA.link, 
        file=paste0(outPath,"TCGA.LIHC.RNA_miRNA.link.RData"))
    write.table(TCGA.LIHC.RNA_miRNA.link, sep="\t", quote=FALSE,
        col.names=TRUE, row.names=FALSE, 
        file=paste0(outPath,"TCGA.LIHC.RNA_miRNA.link.txt"))



    #   5.  Methylation data for genes in RNAseq plot data
    #   ===============================================================
    #
    load("TCGA.LIHC.Plot.Data/TCGA.LIHC.Methylation.Plot.Data.RData")

    geneList <- rownames(TCGA.LIHC.RNAseq.RCircos.Plot.Data);
    methyRow <- which(rownames(TCGA.LIHC.Methylation.Plot.Data) %in% geneList)
    methylData <- TCGA.LIHC.Methylation.Plot.Data[methyRow,]
    methylData <- methylData[order(rownames(methylData)),]

    methylAnnot <- methylData[, c(2, 4, 5, 1)]
    colnames(methylAnnot) <- c("Chromosome","ChromStart","ChromEnd","GeneName")
    methylValue <- methylData[, -c(1:5)]
    TCGA.LIHC.Methyl.RCircos.Plot.Data <- data.frame(methylAnnot, methylValue)

    save(TCGA.LIHC.Methyl.RCircos.Plot.Data, 
        file=paste0(outPath, "TCGA.LIHC.Methyl.RCircos.Plot.Data.RData"))
    write.table(TCGA.LIHC.Methyl.RCircos.Plot.Data, sep="\t", quote=FALSE,
        row.names=TRUE, col.names=TRUE, 
        file=paste0(outPath,"TCGA.LIHC.Methyl.RCircos.Plot.Data.txt"))


    #   6.  CNV data fro genes in RNAseq plot data
    #   ===============================================================
    #
    load("TCGA.LIHC.Plot.Data/TCGA.LIHC.CNV.Plot.Data.RData")

    CNVRow <- which(rownames(TCGA.LIHC.CNV.Plot.Data) %in% geneList)
    CNVData <- TCGA.LIHC.CNV.Plot.Data[CNVRow,]
    CNVData <- CNVData[order(rownames(CNVData)),]

    CNVAnnot <- CNVData[, c(2, 4, 5, 1)]
    colnames(CNVAnnot) <- c("Chromosome","ChromStart","ChromEnd","GeneName")
    CNVSeg   <- CNVData[, -c(1:5)]
    TCGA.LIHC.CNV.RCircos.Plot.Data <- data.frame(CNVAnnot, CNVSeg)

    save(TCGA.LIHC.CNV.RCircos.Plot.Data, 
        file=paste0(outPath, "TCGA.LIHC.CNV.RCircos.Plot.Data.RData"))
    write.table(TCGA.LIHC.CNV.RCircos.Plot.Data, sep="\t", quote=FALSE,
        row.names=TRUE, col.names=TRUE, 
        file=paste0(outPath,"TCGA.LIHC.CNV.RCircos.Plot.Data.txt"))


#   End of TCGA.LIHC.RCircos.Plot.Data.RCircos
#   ======================================================================
   
    

    



























    