#
#   ComplexHeatmap plot
#
#   1.  List of heatmaps
#   2.  OncoPrint
#   ________________________________________________________________
#   <ComplexHeatmap><ComplexHeatmap><ComplexHeatmap><ComplexHeatmap>

library(ComplexHeatmap)


#   list of two heatmap: top differentially expressed RNA and miRNA
#   ==============================================================

load("ComplexHeatmap.Plot/TCGA.LIHC.RNAseq.ComplexHeatmap.Data.RData")
load("ComplexHeatmap.Plot/TCGA.LIHC.miRNAseq.ComplexHeatmap.Data.RData")

RNAseq   <- as.matrix(TCGA.LIHC.RNAseq.ComplexHeatmap.Data)
miRNAseq <- as.matrix(TCGA.LIHC.miRNAseq.ComplexHeatmap.Data)

dim(RNAseq)       #   [1] 114  52
dim(miRNAseq)     #   [1] 114  52

ht_global_opt(heatmap_column_names_gp = gpar(fontsize = 5),
              heatmap_row_names_gp = gpar(fontsize = 4)) 
tissueRNA <- HeatmapAnnotation(
    df=data.frame(RNAseqSample=c(rep("LiverHCC", 26), rep("Normal", 26))), 
    col=list(RNAseqSample=c("LiverHCC"="red", "Normal"= "blue"))) 
tissueMIR <- HeatmapAnnotation(
    df=data.frame(miRNAseqSample=c(rep("LiverHCC", 26), rep("Normal", 26))), 
    col=list(miRNAseqSample=c("LiverHCC"="red", "Normal"= "blue"))) 

heatmapRNA <- Heatmap(TCGA.LIHC.RNAseq.ComplexHeatmap.Data, name="RNA",
                column_title="RNAseq", top_annotation=tissueRNA)
heatmapMIR <- Heatmap(TCGA.LIHC.miRNAseq.ComplexHeatmap.Data, name="miRNA",
                column_title="miRNAseq", top_annotation=tissueMIR)
heatmapList <- heatmapRNA + heatmapMIR

#   pdf("ComplexHeatmap.Plot/TCGA.LIHC.Data.ComplexHeatmap.Plot.pdf", 
#       height=8, width=12)

#   tiff("ComplexHeatmap.Plot/TCGA.LIHC.Data.ComplexHeatmap.Plot.tif", 
#       height=8, width=12,unit="in", res=300, type="windows")

draw(heatmapList, gap=unit(1, "cm"))

#   dev.off()


#   OncoPrint
#   ================================================================

load("ComplexHeatmap.Plot/TCGA.LIHC.OncoPrint.Data.RData")

unique(as.vector(TCGA.LIHC.OncoPrint.Data))

#   [1] "  "                 "Missense_Mutation;" "Silent;"           
#   [4] "Nonsense_Mutation;" "In_Frame_Ins;"      "Frame_Shift_Ins;"  
#   [7] "Frame_Shift_Del;"   "In_Frame_Del;"      "Splice_Site;" 

alter_fun_list <- list(
    background = function(x, y, w, h) {
        grid.rect(x, y, w-unit(0.5, "mm"), h-unit(0.5, "mm"), 
            gp=gpar(fill="#CCCCCC", col=NA))
    },
    Silent = function(x, y, w, h) {
        grid.rect(x, y, w-unit(0.5, "mm"), h-unit(0.5, "mm"), 
            gp=gpar(fill = "blue", col=NA))
    },
    Missense_Mutation = function(x, y, w, h) {
        grid.rect(x, y, w-unit(0.5, "mm"), h-unit(0.5, "mm"), 
            gp = gpar(fill="red", col=NA))
    },
    Nonsense_Mutation = function(x, y, w, h) {
        grid.rect(x, y, w-unit(0.5, "mm"), h*0.33, 
            gp = gpar(fill = "green", col = NA))
    },
    In_Frame_Ins= function(x, y, w, h) {
        grid.rect(x, y, w-unit(0.5, "mm"), h*0.33, 
            gp = gpar(fill = "brown", col = NA))
    },
    Frame_Shift_Ins= function(x, y, w, h) {
        grid.rect(x, y, w-unit(0.5, "mm"), h*0.33, 
            gp = gpar(fill = "cyan", col = NA))
    },  
    Frame_Shift_Del= function(x, y, w, h) {
        grid.rect(x, y, w-unit(0.5, "mm"), h*0.33, 
            gp = gpar(fill = "gold", col = NA))
    },   
    In_Frame_Del= function(x, y, w, h) {
        grid.rect(x, y, w-unit(0.5, "mm"), h*0.33, 
            gp = gpar(fill = "magenta", col = NA))
    },      
    Splice_Site= function(x, y, w, h) {
        grid.rect(x, y, w-unit(0.5, "mm"), h*0.33, 
            gp = gpar(fill = "black", col = NA))
    } 
)

col = c("Silent"="blue", "Missense_Mutation"="red", 
        "Nonsense_Mutation"="green", "In_Frame_Ins"="brown", 
        "Frame_Shift_Ins"="cyan", "Frame_Shift_Del"="gold",
        "In_Frame_Del"="magenta", "Splice_Site"="black")
ht_global_opt(heatmap_column_names_gp = gpar(fontsize = 12),
              heatmap_row_names_gp = gpar(fontsize = 12))

#   pdf("TCGA.LIHC.Mutation.Data.OncoPrint.pdf", height=8, width=12)
#   tiff("TCGA.LIHC.Mutation.Data.OncoPrint.tif", height=8, width=12,
#           unit="in", res=300, type="windows")
oncoPrint(TCGA.LIHC.OncoPrint.Data, get_type=function(x) strsplit(x, ";")[[1]],
    alter_fun_list=alter_fun_list, col=col, show_column_names=TRUE,
    column_title="OncoPrint for TCGA Liver HCC Mutation Data",
    heatmap_legend_param=list(title = "Mutation Type", 
        at=c("Silent", "Missense_Mutation", "Nonsense_Mutation", 
            "In_Frame_Ins", "Frame_Shift_Ins", "Frame_Shift_Del",
            "In_Frame_Del", "Splice_Site"), 
        labels = c("Silent", "Missense_Mutation", "Nonsense_Mutation", 
            "In_Frame_Ins", "Frame_Shift_Ins", "Frame_Shift_Del",
            "In_Frame_Del", "Splice_Site"))
          )

#   dev.off()

#   End of ComplexHeatmap.Plot.RData
#   ====================================================================




