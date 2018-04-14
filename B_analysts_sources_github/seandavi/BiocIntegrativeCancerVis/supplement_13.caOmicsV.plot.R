#
#   caOmicsV plot with both bioMetrix and bioNetCircos layout
#
#   names(biomatrixPlotDemoData)
#   [1] "sampleNames"     "geneNames"       "secondGeneNames" "sampleInfo"      
#   [5] "heatmapData"     "categoryData"    "binaryData"      "summaryInfo"  
#
#   ______________________________________________________________________
#   <caOmicsV><caOmicsV><caOmicsV><caOmicsV><caOmicsV><caOmicsV><caOmicsV>



load("caOmicsV.Plot/TCGA.LIHC.CNV.caOmicsV.Plot.Data.RData")
load("caOmicsV.Plot/TCGA.LIHC.methyCatogery.caOmicsV.Plot.Data.RData")
load("caOmicsV.Plot/TCGA.LIHC.Methylation.caOmicsV.Plot.Data.RData")
load("caOmicsV.Plot/TCGA.LIHC.miRNA.caOmicsV.Plot.Data.RData")
load("caOmicsV.Plot/TCGA.LIHC.Mutation.caOmicsV.Plot.Data.RData")
load("caOmicsV.Plot/TCGA.LIHC.RNA2MIR.Corr.caOmicsV.Plot.Data.RData")
load("caOmicsV.Plot/TCGA.LIHC.RNA2miRNA.Link.caOmicsV.Plot.Data.RData")
load("caOmicsV.Plot/TCGA.LIHC.RNAseq.caOmicsV.Plot.Data.RData")
load("caOmicsV.Plot/TCGA.LIHC.SampleInfo.caOmicsV.Plot.Data.RData")
load("caOmicsV.Plot/TCGA.LIHC.TN_logFC.caOmicsV.Plot.Data.RData")

cnvSegMean  <- TCGA.LIHC.CNV.caOmicsV.Plot.Data
methyStatus <- TCGA.LIHC.methyCatogery.caOmicsV.Plot.Data
methylation <- TCGA.LIHC.Methylation.caOmicsV.Plot.Data
miRNAseq    <- TCGA.LIHC.miRNA.caOmicsV.Plot.Data
mutation    <- TCGA.LIHC.Mutation.caOmicsV.Plot.Data
RNA2mirCorr <- TCGA.LIHC.RNA2MIR.Corr.caOmicsV.Plot.Data
RNA2mirLink <- TCGA.LIHC.RNA2miRNA.Link.caOmicsV.Plot.Data
RNAseq      <- TCGA.LIHC.RNAseq.caOmicsV.Plot.Data
sampleInfo  <- TCGA.LIHC.SampleInfo.caOmicsV.Plot.Data
logFC       <- TCGA.LIHC.TN_logFC.caOmicsV.Plot.Data


#   Make sure all entries in each data set are in same order
#   ========================================================

sampleNames <- as.character(sampleInfo$sampleID)
geneNames <- as.character(RNA2mirLink$geneSymbol)
secondGeneNames <- as.character(RNA2mirLink$miRNA_ID)

sum(rownames(RNAseq) == geneNames)
sum(miRNAseq$geneSymbol == geneNames)
sum(rownames(methyStatus) == geneNames)
sum(rownames(methylation) == geneNames)
sum(rownames(mutation) == geneNames)
sum(rownames(cnvSegMean) == geneNames)
sum(RNA2mirCorr$geneSymbol == geneNames)
sum(logFC$geneSymbol == geneNames)

sum(colnames(RNAseq)[-1] == sampleNames)
sum(colnames(miRNAseq)[-1] == sampleNames)
sum(colnames(methyStatus)[-1] == sampleNames)
sum(colnames(methylation)[-1] == sampleNames)
sum(colnames(mutation)[-1] == sampleNames)
sum(colnames(cnvSegMean)[-1] == sampleNames)

miRNAs <- gsub(".{1,}\\|", "", rownames(miRNAseq))
sum(secondGeneNames == miRNAs)

miRNAs <- gsub(".{1,}\\|","",rownames(RNA2mirCorr))
sum(secondGeneNames == miRNAs)



#   Re-order all data sets by genes based on log2FC
#   ===============================================

geneOrder   <- order(logFC$logFC, decreasing=TRUE)

logFC       <- logFC[geneOrder,]
RNAseq      <- RNAseq[geneOrder,]
miRNAseq    <- miRNAseq[geneOrder,]
methyStatus <- methyStatus[geneOrder,]
mutation    <- mutation[geneOrder,]
methylation <- methylation[geneOrder,]
cnvSegMean  <- cnvSegMean[geneOrder,]
RNA2mirCorr <- RNA2mirCorr[geneOrder,]
RNA2mirLink <- RNA2mirLink[geneOrder,]

sampleNames     <- as.character(sampleInfo$sampleID)
geneNames       <- as.character(RNA2mirLink$geneSymbol)
secondGeneNames <- as.character(RNA2mirLink$miRNA_ID)



#   Put all data in eSet
#   ===================================================
#
library(caOmicsV)
plotData <- getPlotDataSet(
    sampleNames=sampleNames,
    geneNames=geneNames, 
    sampleData=sampleInfo, 
    heatmapData=list(RNAseq, miRNAseq), 
    categoryData=list(methyStatus), 
    binaryData=list(mutation), 
    summaryData=list(logFC), 
    secondGeneNames=secondGeneNames
    )


#   Matrix layout plot
#   ==============================================================

#   pdf("caOmicsV.Plot/TCGA.LIHC.Data.caOmicsV.biomatrix.Plot.pdf", 
#       height=18, width=15)

#   tiff("caOmicsV.Plot/TCGA.LIHC.Data.caOmicsV.bioMatrix.Plot.tiff", 
#       height=18, width=15, unit="in", res=300, type="windows")

par(mai=c(0.5, 1, 0.5, 1))
set.seed(1234)
plotBioMatrix(plotData, summaryType="text", summarybyRow=TRUE, 
        heatmapMax=NULL, heatmapMin=NULL, heatmapColor="BlueWhiteRed")
bioMatrixLegend(heatmapNames=c("RNASeq", "miRNASeq"), 
    categoryNames=c("Methyl H", "Methyl L"), binaryNames="Mutation",   
    heatmapMin=-3, heatmapMax=3, colorType="BlueWhiteRed")

#   dev.off()


#   Network layout plot
#   ==============================================================

plotData <- getPlotDataSet(
    sampleNames=sampleNames,
    geneNames=geneNames, 
    secondGeneNames=secondGeneNames, 
    sampleData=sampleInfo, 
    heatmapData=list(RNAseq, miRNAseq), 
    categoryData=list(methylation), 
    binaryData=list(cnvSegMean)
    )


#   pdf("caOmicsV.Plot/TCGA.LIHC.Data.caOmicsV.bioNetCircos.Plot.pdf", 
#        height=10, width=10)

#   tiff("caOmicsV.Plot/TCGA.LIHC.Data.caOmicsV.bioNetCircos.Plot.tiff", 
#           height=10, width=10, unit="in", res=300, type="windows")

set.seed(1234)
plotBioNetCircos(plotData)
dataNames <- c("Tissue Type", "RNASeq", "miRNASeq", "Methylation", "CNV")
bioNetLegend(dataNames)

#   dev.off()




#   few genes
#   ====================================================

shortGeneList <- c("GATM", "MUC13", "SLC16A2", "TTC39A", "CFL2")

subsetRow <- which(rownames(RNAseq) %in% shortGeneList)
subsetRNAseq <- RNAseq[subsetRow,]

mirnaGene <- gsub("\\|.{1,}", "", rownames(miRNAseq))
subsetRow <- which(mirnaGene %in% shortGeneList)
subsetMIR <- miRNAseq[subsetRow,]

subsetRow <- which(rownames(methylation) %in% shortGeneList)
subsetMethy <- methylation[subsetRow,]

subsetRow <- which(rownames(cnvSegMean) %in% shortGeneList)
subsetCNV <- cnvSegMean[subsetRow,]

subsetRow <- which(rownames(logFC) %in% shortGeneList)
subsetLogFC <- logFC[subsetRow,]

shortGeneList <- rownames(subsetRNAseq)
shortMIRList <- gsub(".{1,}\\|", "", rownames(subsetMIR))

plotData <- getPlotDataSet(
    sampleNames=sampleNames,
    geneNames=shortGeneList, 
    secondGeneNames=shortMIRList, 
    sampleData=sampleInfo, 
    heatmapData=list(subsetRNAseq, subsetMIR), 
    categoryData=list(subsetMethy), 
    binaryData=list(subsetCNV), 
    summaryData=list(subsetLogFC)
    )

    
#   pdf("caOmicsV.Plot/TCGA.LIHC.Data.caOmicsV.bioNetCircos.Plot.2.pdf", 
#        height=8, width=8)

#   tiff("caOmicsV.Plot/TCGA.LIHC.Data.caOmicsV.bioNetCircos.Plot.2.tiff", 
#           height=8, width=8, unit="in", res=300, type="windows")

par(mai=c(1, 1.5, 1, 1))
set.seed(1234)
plotBioNetCircos(plotData)
dataNames <- c("Tissue Type", "RNASeq", "miRNASeq", "Methylation", "CNV")
bioNetLegend(dataNames, heatmapCoor=c(0, 100), textCoor=c(0, 60), 
    scaleWidth=200, scaleHeight=20)

#   dev.off()


#   Adjust legend position manually
#   ============================================
#   bioNetLegend(dataNames,textCoor=c(300,-300), 
#       heatmapCoor=c(300,-200), 
#       scaleWidth=150, scaleHeight=50)











