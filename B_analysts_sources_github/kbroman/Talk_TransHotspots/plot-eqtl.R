library(qtlpvl)
source("my_ploteQTL.R")
data.folder <- "../AttieData/"
data.folder.2 <- "../eQTLscans/"
if(!file.exists("../Figs")) dir.create("../Figs")
pdf("../Figs/plot-eQTL.pdf", w=9.5, h=6.5)

##############################
# eqtl results
load(paste0(data.folder.2, "eqtl_results.RData"))
# reorganize
# probepos:  chr, pos, cM
probepos <- probepos[,c(1, 3, 2)]
names(probepos) <- c("chr", "pos", "cM")

# peaks: pheno, chr, lod1, pos
peaks <- vector("list", 6)
names(peaks) <- names(maxlod)
for(i in seq(along=peaks))
    peaks[[i]] <- data.frame(pheno=rep(rownames(maxlod[[i]]), 20),
                             chr=rep(colnames(maxlod[[i]]), each=nrow(maxlod[[i]])),
                             lod1=as.numeric(maxlod[[i]]),
                             pos=as.numeric(maxpos[[i]]),
                             stringsAsFactors=FALSE)

load(paste0(data.folder, "f2g.calc.RData"))
marker.info20 <- get.marker.info(f2g, chr=c(1:19,"X"))

tissues <- c("adipose", "gastroc", "hypo", "islet", "kidney", "liver")
lod.thr <- 5

par(col.main="firebrick")
par(mfrow=c(2,3), cex.main=1.8)
for(tissue in tissues[c(4,1:3,5:6)]){
  tmp <- peaks[[tissue]][peaks[[tissue]]$lod1 >= lod.thr, ]
  tmp <- tmp[order(tmp$lod1),]

  col <- 1- tmp$lod1/20
  col[col < 0 ] <- 0

  my_ploteQTL(marker.info20, probepos, phenonames=tmp$pheno,
              chr1=tmp$chr, pos1=tmp$pos, main=tissue, cex=0.3,
              col=rgb(col, col, col))
}


dev.off()
