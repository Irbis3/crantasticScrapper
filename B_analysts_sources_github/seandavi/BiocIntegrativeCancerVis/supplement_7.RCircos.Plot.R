#
#   RCircos plot to show:
#
#   1.  Difference of gene expression (mean) between tumor and normal
#       Difference of miRNA expression (mean) between tumor and normal
#       Difference of methylation (mean) between tumor and normal
#       Link between miRNAs and their most negative linked genes
#
#   2.  for one sample pair (TCGA-BC-A10Q-01A and TCGA-BC-A10Q-11A): 
#
#       Gene expression, miRNA expression, methylation, CNV
#   ________________________________________________________________________
#   <RCircos><RCircos><RCircos><RCircos><RCircos><RCircos><RCircos><RCircos>



#   Load plot data
#   ==========================================================
#
load("RCicos.Plot/TCGA.LIHC.RNAseq.RCircos.Plot.Data.RData")
load("RCicos.Plot/TCGA.LIHC.miRNAseq.RCircos.Plot.Data.RData")
load("RCicos.Plot/TCGA.LIHC.Methyl.RCircos.Plot.Data.RData")
load("RCicos.Plot/TCGA.LIHC.CNV.RCircos.Plot.Data.RData")
load("RCicos.Plot/TCGA.LIHC.RNA_miRNA.link.RData")
load("RCicos.Plot/UCSC.HG38.Human.CytoBandIdeogram.RData")          

library(RCircos)


#   Plot 1
#   ========================================================

#   data to be plotted. Means of RNAseq, miRNAseq, methylation
#   of tumor and normal tissue, gene lables, and link lines
#
RNAseqData <- TCGA.LIHC.RNAseq.RCircos.Plot.Data
RNAseq <- data.frame(RNAseqData[, 1:4],
    tumorExpr=rowMeans(as.matrix(RNAseqData[, 5:30])),
    normalExpr=rowMeans(as.matrix(RNAseqData[, 31:56]))
)

miRNAseqData <- TCGA.LIHC.miRNAseq.RCircos.Plot.Data
miRNAseq <- data.frame(miRNAseqData[, 1:4],
    tumorMIR=rowMeans(as.matrix(miRNAseqData[, 5:30])),
    normalMIR=rowMeans(as.matrix(miRNAseqData[, 31:56]))
)

methylData <- TCGA.LIHC.Methyl.RCircos.Plot.Data
methylation <- data.frame(methylData[,1:4],
    tumorMethy=rowMeans(as.matrix(methylData[, 5:30])),
    normalMethy=rowMeans(as.matrix(methylData[, 31:56]))
)

miRNA2RNA <- TCGA.LIHC.RNA_miRNA.link
geneAxis <- miRNA2RNA[,c(2:4,1)]
mirAxis <- miRNA2RNA[, c(6:8,5)]

mirAxis <- mirAxis[-which(duplicated(mirAxis[,4])),]
colnames(mirAxis) <- colnames(geneAxis)
mirAxis$Gene <- gsub("hsa-", "", mirAxis$Gene)

geneLabelData <- rbind(geneAxis, mirAxis)
geneLabelData["PlotColor"] <- c(rep("blue", nrow(geneAxis)), 
            rep("red", nrow(mirAxis)))
            
geneLink <- miRNA2RNA[, -c(1,5)]
geneLink["PlotColor"] <- c(rep("blue", 3), rep("red", 4), 
            rep("green", 4), rep("magenta", 3))

#   RCircos plot
#

hg38 <- UCSC.HG38.Human.CytoBandIdeogram
chromosomes <- paste0("chr", c(1:22, "X", "Y"))
hg38 <- hg38[which(hg38$Chromosome %in% chromosomes), ]

RCircos.Set.Core.Components(hg38, chr.exclude=NULL, 
    tracks.inside=14, tracks.outside=0)

params <- RCircos.Get.Plot.Parameters()
params$heatmap.width <- 400
params$hist.width <- 400
RCircos.Reset.Plot.Parameters(params)

# pdf("RCicos.Plot/TCGA.LIHC.Data.RCircos.Plot.pdf", heigh=8, width=8)
#   tiff("RCicos.Plot/TCGA.LIHC.Data.RCircos.Plot.tiff", 
#       heigh=8, width=8,unit="in", res=300, type="windows")
    
RCircos.Set.Plot.Area()
RCircos.Chromosome.Ideogram.Plot()

RCircos.Heatmap.Plot(RNAseq, data.col=5, track.num=1, side="in")
RCircos.Heatmap.Plot(RNAseq, data.col=6, track.num=2, "in")

RCircos.Heatmap.Plot(miRNAseq, data.col=5, track.num=3.5, "in")
RCircos.Heatmap.Plot(miRNAseq, data.col=6, track.num=4.5, "in")

RCircos.Histogram.Plot(methylation, data.col=5, track.num=6, "in")
RCircos.Histogram.Plot(methylation, data.col=6, track.num=7, "in")

RCircos.Gene.Connector.Plot(geneLabelData, track.num=8, "in")
RCircos.Gene.Name.Plot(geneLabelData, name.col=4, track.num=9, "in")

RCircos.Link.Plot(geneLink, track.num=11, by.chromosome=FALSE)

textLabel <- c("From outside to center:\n", 
                "Chromosome ideogram\n",
                "RNAseq: tumor, normal\n",
                "miRNAseq: tumor, normal\n",
                "Methylation: tumor, normal\n",
                "Gene names\n",
                "miRNA RNA link")
legend("topright", legend=textLabel, cex=0.4)

# dev.off()


#   Plot 2
#   =============================================================

#   Plot data form one sample pair
#
sampleID <- c("TCGA.BC.A10Q.01A", "TCGA.BC.A10Q.11A")

RNAseqData <- TCGA.LIHC.RNAseq.RCircos.Plot.Data
colNum <- which(colnames(RNAseqData) %in% sampleID)
RNAseq <- RNAseqData[, c(1:4, colNum)]

miRNAseqData <- TCGA.LIHC.miRNAseq.RCircos.Plot.Data
colNum <- which(colnames(miRNAseqData) %in% sampleID)
miRNAseq <- miRNAseqData[, c(1:4, colNum)]

methylData <- TCGA.LIHC.Methyl.RCircos.Plot.Data
colNum <- which(colnames(methylData) %in% sampleID)
methylation <- methylData[,c(1:4, colNum)]

CNVData <- TCGA.LIHC.CNV.RCircos.Plot.Data
colNum <- which(colnames(CNVData) %in% sampleID)
cnvData <- CNVData[,c(1:4, colNum)]

geneFC <- RNAseq[,5]-RNAseq[,6]
geneRow <- which(geneFC < -12)
mirFC  <- miRNAseq[,5] -miRNAseq[,6]
mirRow <- which(abs(mirFC)>=4)
geneLabelData <- rbind(RNAseq[geneRow,1:4], miRNAseq[mirRow,1:4])
geneLabelData[,4] <- gsub("hsa-", "", geneLabelData[,4])


#   RCircos plot
#
RCircos.Set.Core.Components(hg38, chr.exclude=NULL, 
    tracks.inside=15, tracks.outside=0)

params <- RCircos.Get.Plot.Parameters()
params$heatmap.width <- 400
params$hist.width <- 400
params$point.size <- 1.5
params$track.background <- NA
RCircos.Reset.Plot.Parameters(params)

# pdf("RCicos.Plot/TCGA.LIHC.Data.RCircos.Plot.2.pdf", heigh=8, width=8)
   tiff("RCicos.Plot/TCGA.LIHC.Data.RCircos.Plot.2.tiff", 
       heigh=8, width=8,unit="in", res=300, type="windows")

RCircos.Set.Plot.Area()
RCircos.Chromosome.Ideogram.Plot()

RCircos.Gene.Connector.Plot(geneLabelData, track.num=1, side="in")
RCircos.Gene.Name.Plot(geneLabelData, name.col=4, track.num=2, side="in")

RCircos.Heatmap.Plot(RNAseq, data.col=5, track.num=6, side="in")
RCircos.Heatmap.Plot(RNAseq, data.col=6, track.num=7, side="in")

RCircos.Heatmap.Plot(miRNAseq, data.col=5, track.num=8.5, side="in")
RCircos.Heatmap.Plot(miRNAseq, data.col=6, track.num=9.5, side="in")

RCircos.Histogram.Plot(methylation, data.col=5, track.num=11, side="in")
RCircos.Histogram.Plot(methylation, data.col=6, track.num=12, side="in")

RCircos.Scatter.Plot(cnvData, data.col=5, track.num=13.5, side="in", by.fold=1)
RCircos.Scatter.Plot(cnvData, data.col=6, track.num=14.5, side="in", by.fold=1)

dev.off()


#   End of RCircos.Plot.RCicos
#   =======================================================================