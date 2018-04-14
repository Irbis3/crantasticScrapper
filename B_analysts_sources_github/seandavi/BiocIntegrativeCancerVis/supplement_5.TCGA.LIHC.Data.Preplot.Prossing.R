#
#   Preparation of plot data from TCGA Liver HCC (LIHC) data.
#   We are going to dispay the following data sets: 
#   
#   1). RNAseq data of most significant deferentially expressed genes
#   2). miRNA data which are most negative correlated to genes above
#   3). Methylation of genes above
#   4). link table between genes and miRNAs above
#   5). Mutations that are in the genomic region of genes above
#   6). CNV data in the genomic region of genes above 
#
#   Last modified on May 11, 2016.
#  _____________________________________________________________________________
#  <Plot data><Plot data><Plot data><Plot data><Plot data><Plot data><Plot data>


    library(GenomicRanges)
    filePath <- "TCGA.LIHC.Data.Processed/"
    outPath  <- "TCGA.LIHC.Plot.Data/"
    dir.create(outPath)



#   1.  Annotation information for gene and miRNA
#   ===============================================================

    load("TCGAbiolinks.Download/biomaRt.Gene.Annotation.GRCh38.p5.RData")
    load("TCGAbiolinks.Download/biomaRt.miRNA.Annotation.GRCh38.p5.RData")


#   2.  Gene list of significantly differentially expressed between 
#       tumors and normals (>=2 fold change and fdr<=0.01)
#   ==============================================================

    load(paste0(filePath, "TCGA.LIHC.RNAseqV2.lmfit.result.Rdata"))

    absLogFC <- abs(TCGA.LIHC.RNAseqV2.lmfit.result$logFC)
    highFC <- which(absLogFC >= 1) 

    sigGene <- TCGA.LIHC.RNAseqV2.lmfit.result[highFC,]
    sigRow <- which(sigGene$adj.P.Val <= 0.01)

    geneList <- rownames(sigGene[sigRow,])
    geneList <- gsub("\\|.{1,}", "", geneList)
    geneList <- unique(geneList)  
    geneList <- geneList[-which(geneList=="?")]

    length(geneList)    #   [1] 1890

    #   keep genes which have annotation information only
    #
    annotRow <- which(biomaRt.Gene.Annotation$hgnc_symbol %in% geneList)
    geneAnnot <- biomaRt.Gene.Annotation[annotRow, ]
    rownames(geneAnnot) <- geneAnnot[, 1]
    geneAnnot <- geneAnnot[order(rownames(geneAnnot)),]

    geneList <- rownames(geneAnnot)


#   2.  RNAseq data subset ( >=2 fold change with adjusted.p <= 0.01)
#   =================================================================

    load(paste0(filePath, "Normalized.TCGA.LIHC.RNAseqV2.Counts.RData"))
    RNAseq <- normalized.TCGA.LIHC.RNAseq.Counts$E

    geneNames <- gsub("\\|.{1,}", "", rownames(RNAseq))
    geneRows <- which(geneNames %in% geneList)
    RNAseqSubset <- RNAseq[geneRows,]
    rownames(RNAseqSubset) <- geneNames[geneRows]
    
    rowOrder <- order(rownames(RNAseqSubset))
    RNAseqSubset <- RNAseqSubset[rowOrder,]
    
    dim(RNAseqSubset)                           #   [1] 1772   52
    sum(rownames(RNAseqSubset) == geneList)     #   [1] 1772
    
    TCGA.LIHC.RNAseq.Plot.Data <- data.frame(geneAnnot,RNAseqSubset)
    save(TCGA.LIHC.RNAseq.Plot.Data, 
        file=paste0(outPath, "TCGA.LIHC.RNAseq.Plot.Data.RData"))
    write.table(TCGA.LIHC.RNAseq.Plot.Data, sep="\t",
        quote=FALSE, row.names=FALSE, col.names=TRUE,
        file=paste0(outPath, "TCGA.LIHC.RNAseq.Plot.Data.txt"))


#   3.  Correlation table for RNAseq and miRNAs most negative related 
#       to differentially expressed genes 
#   =================================================================

    load(paste0(filePath, "Normalized.TCGA.LIHC.miRNAseqV2.Counts.RData"))

    RNAseq <- TCGA.LIHC.RNAseq.reads.2fold.asj_p.01
    miRNAseq <- normalized.TCGA.LIHC.miRNA.Counts$E

    miRNAID=rep("", nrow(RNAseq))
    tumor.corr.values=rep(0, nrow(RNAseq))
    tumor.p.values=rep(1, nrow(RNAseq))
    normal.corr.values=rep(0, nrow(RNAseq))
    normal.p.values=rep(1, nrow(RNAseq))

    for(aGene in seq_len(nrow(RNAseq)))
    {
        print(rownames(RNAseq)[aGene])
        RNA.tumor  <- as.numeric(RNAseq[aGene, 1:26]);
        RNA.normal <- as.numeric(RNAseq[aGene, 27:52]);

        tumor.corr.list <- rep(0, nrow(miRNAseq))
        tumor.p.list    <- rep(1, nrow(miRNAseq))

        normal.corr.list <- rep(0, nrow(miRNAseq))
        normal.p.list    <- rep(1, nrow(miRNAseq))

        for(aMir in seq_len(nrow(miRNAseq)))
        {
            miRNA.tumor <- as.numeric(miRNAseq[aMir,1:26])
            miRNA.normal <- as.numeric(miRNAseq[aMir, 27:52])
        
            res <- cor.test(RNA.tumor, miRNA.tumor, method="pearson")
            tumor.corr.list[aMir] <- res$estimate
            tumor.p.list[aMir] <- res$p.value
        
            res <- cor.test(RNA.normal, miRNA.normal, method="pearson")
            normal.corr.list[aMir] <- res$estimate
            normal.p.list[aMir] <- res$p.value
        }
        index <- which(tumor.corr.list == min(tumor.corr.list))
        if(length(index) > 1) index <- index[1]
        miRNAID[aGene] <- rownames(miRNAseq)[index]
    
        tumor.corr.values[aGene] <- tumor.corr.list[index]
        tumor.p.values[aGene] <- tumor.p.list[index]
    
        normal.corr.values[aGene] <- normal.corr.list[index]
        normal.p.values[aGene] <- normal.p.list[index]
    }

    tumor.fdr <- p.adjust(tumor.p.values, method="BH");
    normal.fdr <- p.adjust(normal.p.values, method="BH");
    TCGA.LIHC.RNAseq_miRNAseq_corr <- data.frame(
            geneSymbol=rownames(RNAseq),
            miRNA_ID=miRNAID, 
            tumor.corr.values=tumor.corr.values,
            tumor.p.values=tumor.p.values, 
            tumor.fdr=tumor.fdr,
            normal.corr.values=normal.corr.values,
            normal.p.values=normal.p.values, 
            normal.fdr=normal.fdr
    )

    save(TCGA.LIHC.RNAseq_miRNAseq_corr, file=paste0(outPath,
            "TCGA.LIHC.RNAseq_miRNAseq_corr_by_group.RData"))
    write.table(TCGA.LIHC.RNAseq_miRNAseq_corr, sep="\t", 
        quote=FALSE, row.names=FALSE, col.names=TRUE,
        file=paste0(outPath,"TCGA.LIHC.RNAseq_miRNAseq_corr_by_group.txt"))


#   4.   miRNAseq data subset
#   =================================================================

    annotMIR <- as.character(biomaRt.miRNA.Annotation$mirbase_id)
    annotRow <- which(annotMIR %in% rownames(miRNAseq))
    mirAnnot <- biomaRt.miRNA.Annotation[annotRow,]
    rownames(mirAnnot) <- as.character(mirAnnot$mirbase_id)
    mirAnnot <- mirAnnot[order(rownames(mirAnnot)), ]
    dim(mirAnnot)   #   [1] 189   5

    mirID <- as.character(mirAnnot$mirbase_id)
    
    mirRow <- which(rownames(miRNAseq) %in% mirID)
    mirSubset <- miRNAseq[mirRow, ]
    mirSubset <- mirSubset[order(rownames(mirSubset)), ]
    sum(rownames(mirSubset) == mirID)   #   [1] 189
    
    TCGA.LIHC.miRNAseq.Plot.Data <- data.frame(mirAnnot,mirSubset)
    rownames(TCGA.LIHC.miRNAseq.Plot.Data) <- mirID
    
    save(TCGA.LIHC.miRNAseq.Plot.Data, 
        file=paste0(outPath, "TCGA.LIHC.miRNAseq.Plot.Data.RData"))
    write.table(TCGA.LIHC.miRNAseq.Plot.Data, sep="\t", quote=FALSE,
        row.names=FALSE, col.names=TRUE, 
        file=paste0(outPath, "TCGA.LIHC.miRNAseq.Plot.Data.txt"))


#   5.   Methylation data subset
#   =================================================================

    load(paste0(filePath, "Normalized.TCGA.LIHC.Methylation.Values.RData"))
    
    methyGene <- rownames(normalized.TCGA.LIHC.Methylation.Values)
    methyGene <- gsub(".{1,}\\|", "", methyGene)
    methyGene <- gsub(";.{1,}", "", methyGene)

    #   Reduce total gene number just for convenience
    #
    redundantRow <- which(duplicated(methyGene))
    methySubset <- normalized.TCGA.LIHC.Methylation.Values[-redundantRow, ]
    rownames(methySubset) <- methyGene[-redundantRow]
    dim(methySubset)    #   [1] 20331    52
 
    annotRows <- which(rownames(geneAnnot) %in% rownames(methySubset)) 
    methyAnnot <- geneAnnot[annotRows,]
    
    methyRows <- which(rownames(methySubset) %in% rownames(methyAnnot))
    methySubset <- methySubset[methyRows,]

    methyAnnot <- methyAnnot[order(rownames(methyAnnot)),]
    methySubset <- methySubset[order(rownames(methySubset)),]
    dim(methySubset)    #   [1] 1732   52
    sum(rownames(methyAnnot) == rownames(methySubset))  #   [1] 1732
    
    TCGA.LIHC.Methylation.Plot.Data <- data.frame(methyAnnot, methySubset)
  
    save(TCGA.LIHC.Methylation.Plot.Data, 
        file=paste0(outPath, "TCGA.LIHC.Methylation.Plot.Data.RData"))
    write.table(TCGA.LIHC.Methylation.Plot.Data, sep="\t", quote=FALSE,
        col.names=TRUE, row.names=FALSE,
        file=paste0(outPath, "TCGA.LIHC.Methylation.Plot.Data.txt"))



#   6.  CNV data re-formatted as rows for genes and columns for samples
#   ===================================================================
    library(GenomicRanges);    

    finalGenes <- GRanges(seqnames=as.character(geneAnnot$chromosome_name),
                    ranges=IRanges(start=as.numeric(geneAnnot$start_position),
                                end=as.numeric(geneAnnot$end_position)),
                    strand=geneAnnot$strand, names=geneAnnot$hgnc_symbol);
    seqlevels(finalGenes) <- paste0("chr", c(1:22, "X", "Y"))
    
    plotSamples <- colnames(RNAseq);
    
    load("TCGAbiolinks.Download/TCGAbiolinks.LIHC.CNV_SNP.seg_mean.RData");
    
    CNV <- lihc.CNV_SNP.data;
    CNV$Chromosome <- paste0("chr", CNV$Chromosome);
    CNV$Sample <- substr(CNV$Sample, 1, 16)
    TCGA.LIHC.CNV.Plot.Data <- data.frame(geneName=geneAnnot$hgnc_symbol) 
            
    for(aSam in seq_along(plotSamples))
    {
        sampleID <- plotSamples[aSam];  
        if(!sampleID %in% CNV$Sample) next;
        print(sampleID);

        rowIndex <- grep(sampleID, CNV$Sample);   
        cnvRanges <- GRanges(seqnames=as.character(CNV$Chromosome[rowIndex]),
                    ranges=IRanges(start=as.numeric(CNV$Start[rowIndex]),
                                end=as.numeric(CNV$End[rowIndex])),
                    seqMean=as.numeric(CNV$Segment_Mean[rowIndex]));
        seqlevels(cnvRanges) <- paste0("chr", c(1:22, "X", "Y"))

        hit <- as.matrix(findOverlaps(finalGenes, cnvRanges))
        geneHit <- unique(hit[,1])

        segment <- as.numeric(rep(1, length(finalGenes)));    
        for(aHit in seq_along(geneHit))
        {
            segRows <- hit[which(hit[,1] == geneHit[aHit]),2]
            segment[geneHit[aHit]] <- mean(cnvRanges$seqMean[segRows])
        }
        TCGA.LIHC.CNV.Plot.Data[sampleID] <- segment;
    }
    
    TCGA.LIHC.CNV.Plot.Data<-data.frame(geneAnnot, TCGA.LIHC.CNV.Plot.Data[,-1])
    save(TCGA.LIHC.CNV.Plot.Data, 
        file=paste0(outPath,"TCGA.LIHC.CNV.Plot.Data.RData"))
    write.table(TCGA.LIHC.CNV.Plot.Data, sep="\t", quote=FALSE, 
        row.names=FALSE, col.names=TRUE, 
        file=paste0(outPath,"TCGA.LIHC.CNV.Plot.Data.txt"))
    


#   7.  Mutation data reformatted as rows for genes and columns for samples.
#       with binary or category data
#   ===================================================================

    load("TCGAbiolinks.Download/TCGAbiolinks.LIHC.Mutation.Level_2.MAF.RData")
    
    totalGenes   <- nrow(geneAnnot);
    totalSamples <- length(colnames(RNAseq));
    
    indelMatrix <- matrix(rep(0, totalGenes*totalSamples), ncol=totalSamples);
    rownames(indelMatrix) <- as.character(geneAnnot$hgnc_symbol);    
    colnames(indelMatrix) <- colnames(RNAseq)
    mutatMatrix <- indelMatrix; 
    
    mutationData <- lihc.Mutation.data
    indelType <- c("DEL", "INS")
    mutationType <- c("Missense_Mutation", "Nonsense_Mutation")
    
    mutatGene <- as.character(mutationData$Hugo_Symbol)
    targetGene  <- rownames(indelMatrix)

    for(aMut in seq_along(mutatGene))
    {
        if(!mutatGene[aMut] %in% targetGene) next;
        print(mutatGene[aGene]);
        
        targetRow <- which(targetGene %in% mutatGene[aMut])
        if(length(targetRow) > 1) stop("Unknown error.")
        
        tumorID <- substr(mutationData$Tumor_Sample_Barcode[aMut], 1, 16)
        targetCol <- which(colnames(indelMatrix) == tumorID)

        if(mutationData[aMut, 10] == "DEL") {
            indelMatrix[targetRow, targetCol] <- -1;
        } else if(mutationData[aMut, 10] == "INS") {
            indelMatrix[targetRow, targetCol] <- 1;        
        } else if( mutationData[aMut, 9] == "Missense_Mutation" ||
            mutationData[aMut, 9] == "Nonsense_Mutation") {
            mutatMatrix[targetRow, targetCol] <- 1;
        } else { 
            print(paste(mutationData$Variant_Type[aMut],"ignored"))
        }
    }
    
    TCGA.LIHC.Indels.Plot.Data <- data.frame(geneAnnot, indelMatrix)
    save(TCGA.LIHC.Indels.Plot.Data, 
        file=paste0(outPath,"TCGA.LIHC.Indels.Plot.Data.RData"))
    write.table(TCGA.LIHC.Indels.Plot.Data, sep="\t", quote=FALSE, 
        row.names=FALSE, col.names=TRUE, 
        file=paste0(outPath,"TCGA.LIHC.Indels.Plot.Data.txt"))
 
    TCGA.LIHC.Mutation.Plot.Data <- data.frame(geneAnnot, mutatMatrix)
    save(TCGA.LIHC.Mutation.Plot.Data, 
        file=paste0(outPath,"TCGA.LIHC.Mutation.Plot.Data.RData"))
    write.table(TCGA.LIHC.Mutation.Plot.Data, sep="\t", quote=FALSE, 
        row.names=FALSE, col.names=TRUE, 
        file=paste0(outPath,"TCGA.LIHC.Mutation.Plot.Data..txt"))
 


    #   miRNA table for RNA-miRNA correlation plot. Each mirna2rna
    #   may link to multiple RNAs (genes) and will be identified
    #   with geneName|miRNA_id for each row
    #   ==========================================================

    mirnaID <- as.character(TCGA.LIHC.RNAseq_miRNAseq_corr$miRNA_ID)
    geneID <- as.character(TCGA.LIHC.RNAseq_miRNAseq_corr$geneSymbol)
    mirnaRow <- which(mirnaID %in% rownames(TCGA.LIHC.miRNAseq.Plot.Data))
    mirnaID <- mirnaID[mirnaRow]
    geneID  <- geneID[mirnaRow] 
    
    aMIR <- 1;
    mirnaRow <- which(rownames(TCGA.LIHC.miRNAseq.Plot.Data) == mirnaID[aMIR])
    mirna2rna <- TCGA.LIHC.miRNAseq.Plot.Data[mirnaRow,]

    for(aMIR in 2:length(mirnaID))
    {
        mirnaRow <- which(rownames(TCGA.LIHC.miRNAseq.Plot.Data)==mirnaID[aMIR])
        mirna2rna <- rbind(mirna2rna, TCGA.LIHC.miRNAseq.Plot.Data[mirnaRow,])
    }

    rownames(mirna2rna) <- paste(geneID, mirnaID, sep="|")
    TCGA.LIHC.miRNA2RNA.Plot.Data <- mirna2rna
    
    save(TCGA.LIHC.miRNA2RNA.Plot.Data, 
        file=paste0(outPath, "TCGA.LIHC.miRNA2RNA.Plot.Data.RData"))
    write.table(TCGA.LIHC.miRNA2RNA.Plot.Data, sep="\t", quote=FALSE, 
        row.names=TRUE, col.names=TRUE,
        file=paste0(outPath, "TCGA.LIHC.miRNA2RNA.Plot.Data.txt"))


#   End of TCGA.LIHC.Data.Plot.Data.RData
#   ===========================================================================
























