#
#   gtrellis plot to show patterns of RNAseq, methylation and copy number 
#   variation for one sample.
#   ______________________________________________________________________
#   <gtrellis><gtrellis><gtrellis><gtrellis><gtrellis><gtrellis><gtrellis>


#   plot data. data.frame with chromosome name, start position, end
#   position, and data values
#   =================================================================

load("gtrellis.Plot/TCGA.LIHC.gtrellis.Plot.data.RNAseq.RData")
load("gtrellis.Plot/TCGA.LIHC.gtrellis.Plot.Data.methylation.RData")
load("gtrellis.Plot/TCGA.LIHC.gtrellis.Plot.Tumor.CNV.RData")
load("gtrellis.Plot/TCGA.LIHC.gtrellis.Plot.Normal.CNV.RData")


#   One tumor sample. Using GenomicRanges object to sort data by 
#   chromosome names in the order of chr1~22, chrX and chrY)
#   ============================================================

library(GenomicRanges)

RNAseq <- GRanges(seqnames=TCGA.LIHC.gtrellis.Plot.RNAseq[[1]],
    ranges=IRanges(start=TCGA.LIHC.gtrellis.Plot.RNAseq[[2]],
                end=TCGA.LIHC.gtrellis.Plot.RNAseq[[3]]),
    counts=TCGA.LIHC.gtrellis.Plot.RNAseq[[4]])
RNAseq <- sortSeqlevels(RNAseq)
RNAseq <- sort(RNAseq)
rnaRange <- range(RNAseq$counts)

methylation <- GRanges(seqnames=TCGA.LIHC.gtrellis.Plot.methylation[[1]],
    ranges=IRanges(start=TCGA.LIHC.gtrellis.Plot.methylation[[2]],
                end=TCGA.LIHC.gtrellis.Plot.methylation[[3]]),
    betaValues=TCGA.LIHC.gtrellis.Plot.methylation[[4]])
methylation <- sortSeqlevels(methylation)
methylation <- sort(methylation)
methyRange <- range(methylation$betaValues)

CNV <- GRanges(seqnames=TCGA.LIHC.gtrellis.Plot.Tumor.CNV[[2]],
    ranges=IRanges(start=TCGA.LIHC.gtrellis.Plot.Tumor.CNV[[3]],
                end=TCGA.LIHC.gtrellis.Plot.Tumor.CNV[[4]]),
    seg_mean=TCGA.LIHC.gtrellis.Plot.Tumor.CNV[[5]])
CNV <- sortSeqlevels(CNV)
CNV <- sort(CNV)
cnvRange <- range(CNV$seg_mean)

sampleName <- colnames(TCGA.LIHC.gtrellis.Plot.RNAseq)[4]
sampleName <- gsub("\\.", "-", sampleName)

#   head(RNAseq)
#   head(miRNAseq)
#   head(methylation)
#   head(CNV)


#   gtrellis plot
#   ====================================================

library(gtrellis)
library(circlize)
library(RColorBrewer)

#   We will plot three tracks: cnv(points), methylation (polygon), 
#   RNAseq (heatmap). Track alignment is from top to bottom 
#   ==============================================================

#   pdf("TCGA.LIHC.Data.gtrellis.Plot.tumor.pdf", height=4, width=24)
#   tiff("TCGA.LIHC.Data.gtrellis.Plot.tumor.tiff", height=4, width=24,
#       unit="in", res=300, type="windows")

gtrellis_layout(n_track=3, species = "hg19", 
    track_axis=c(TRUE,FALSE,TRUE), xaxis = FALSE,  
    track_ylim=c(cnvRange, 0,1,0,1),
    track_height=unit.c(unit(0.5, "null"), unit(0.5, "null"), 
                    unit(0.5, "null")),
    track_ylab=c("CNV", "RNAseq", "Methyl"),
    title=paste("TCGA LIHC", sampleName), xlab = "Chromosomes", 
    add_name_track=TRUE, add_ideogram_track=TRUE)

#   the first track: cnv
#   ====================================================
add_track(CNV, track=2, panel.fun = function(CNV) {
    x=(start(CNV) + end(CNV))/2;
    y=CNV$seg_mean;
    grid.points(x, y, pch=16, size=unit(1, "mm"))
})

#   RNAseq (gene expression)
#   ==========================================================
col_fun = circlize::colorRamp2(seq(min(rnaRange), max(rnaRange), 
                length=11), rev(brewer.pal(11, "RdYlBu")))
add_track(RNAseq, track=3, panel.fun = function(RNAseq) {
    grid.rect(start(RNAseq), 0, width=(end(RNAseq)-start(RNAseq))*100, 
        height=0.9, just=c(0, 0), default.units="native", 
        gp=gpar(fill=col_fun(RNAseq$counts), col = NA))
})

#   methylation
#   =========================================================
add_track(methylation, track=4,panel.fun = function(methylation) {
    x = (start(methylation) + end(methylation))/2
    y = methylation$betaValues
    grid.polygon(c(x[1], x, x[length(x)]), 
        c(0, y, 0), default.units="native", gp=gpar(fill="pink"))
})

#   dev.off()





#   Repeat steps above for a normal sample
#   _______________________________________________________________
#   <normal sample> <normal sample> <normal sample> <normal sample>

RNAseq <- GRanges(seqnames=TCGA.LIHC.gtrellis.Plot.RNAseq[[1]],
    ranges=IRanges(start=TCGA.LIHC.gtrellis.Plot.RNAseq[[2]],
                end=TCGA.LIHC.gtrellis.Plot.RNAseq[[3]]),
    counts=TCGA.LIHC.gtrellis.Plot.RNAseq[[5]])
RNAseq <- sortSeqlevels(RNAseq)
RNAseq <- sort(RNAseq)
rnaRange <- range(RNAseq$counts)

methylation <- GRanges(seqnames=TCGA.LIHC.gtrellis.Plot.methylation[[1]],
    ranges=IRanges(start=TCGA.LIHC.gtrellis.Plot.methylation[[2]],
                end=TCGA.LIHC.gtrellis.Plot.methylation[[3]]),
    betaValues=TCGA.LIHC.gtrellis.Plot.methylation[[5]])
methylation <- sortSeqlevels(methylation)
methylation <- sort(methylation)
methyRange <- range(methylation$betaValues)

CNV <- GRanges(seqnames=TCGA.LIHC.gtrellis.Plot.Normal.CNV[[2]],
    ranges=IRanges(start=TCGA.LIHC.gtrellis.Plot.Normal.CNV[[3]],
                end=TCGA.LIHC.gtrellis.Plot.Normal.CNV[[4]]),
    seg_mean=TCGA.LIHC.gtrellis.Plot.Normal.CNV[[5]])
CNV <- sortSeqlevels(CNV)
CNV <- sort(CNV)
cnvRange <- range(CNV$seg_mean)

sampleName <- colnames(TCGA.LIHC.gtrellis.Plot.RNAseq)[5]
sampleName <- gsub("\\.", "-", sampleName)


#   pdf("TCGA.LIHC.Data.gtrellis.Plot.normal.pdf", height=4, width=24)

#   tiff("TCGA.LIHC.Data.gtrellis.Plot.normal.tiff", height=4, width=24,
#          unit="in", res=300, type="windows")

gtrellis_layout(n_track=3, species = "hg19", 
    track_axis=c(TRUE,FALSE,TRUE), xaxis = FALSE,  
    track_ylim=c(cnvRange, 0,1,0,1),
    track_height=unit.c(unit(0.5, "null"), unit(0.5, "null"), 
                    unit(0.5, "null")),
    track_ylab=c("CNV", "RNAseq", "Methyl"),
    title=paste("TCGA LIHC", sampleName), xlab = "Chromosomes", 
    add_name_track=TRUE, add_ideogram_track=TRUE)


#   the first track: cnv
#   ====================================================
add_track(CNV, track=2, panel.fun = function(CNV) {
    x=(start(CNV) + end(CNV))/2;
    y=CNV$seg_mean;
    grid.points(x, y, pch=16, size=unit(1, "mm"))
})

#   RNAseq (gene expression)
#   ==========================================================
col_fun = circlize::colorRamp2(seq(min(rnaRange), max(rnaRange), 
                length=11), rev(brewer.pal(11, "RdYlBu")))
add_track(RNAseq, track=3, panel.fun = function(RNAseq) {
    grid.rect(start(RNAseq), 0, width=(end(RNAseq)-start(RNAseq))*100, 
        height=0.9, just=c(0, 0), default.units="native", 
        gp=gpar(fill=col_fun(RNAseq$counts), col = NA))
})

#   methylation
#   =========================================================
add_track(methylation, track=4, panel.fun = function(methylation) {
    x = (start(methylation) + end(methylation))/2
    y = methylation$betaValues
    grid.polygon(c(x[1], x, x[length(x)]), 
        c(0, y, 0), default.units="native", gp=gpar(fill="pink"))
})


#   dev.off()


#   End of gtrellis.plt.R
#   ===================================================================












