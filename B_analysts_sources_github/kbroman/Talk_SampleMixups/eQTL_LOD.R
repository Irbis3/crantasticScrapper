library(lineup)
library(qtl)

etraits <- c("499541","10002916257")
loc <- c("1","13")

file <- "eQTL_LOD.RData"
if(file.exists(file)) {
  load(file)
} else {
  cat("loading\n")
  load("~/Projects/Attie/GoldStandard/Genotypes/Study_and_clean/Data/clean_cross.RData")
  load("~/Projects/Attie/GoldStandard/Expression/MLRatios/F2.mlratio.islet.RData")
  id <- findCommonID(f2g$pheno$MouseNum, colnames(islet.mlratio))
  f2g <- f2g[,id$first]
  islet.mlratio <- islet.mlratio[,id$second]
  f2g <- calc.genoprob(f2g, step=0.5, stepwidth="max", err=0.002, map.function="c-f")
  for(i in 1:2)
    f2g$pheno[,etraits[i]] <- islet.mlratio[etraits[i],]
  out <- scanone(f2g, method="hk", phe=etraits, chr="-un")
  oldgmap <- pull.map(f2g)

  load("~/Projects/Attie/GoldStandard/FinalData/aligned_geno_with_pmap.RData")
  load("~/Projects/Attie/GoldStandard/FinalData/islet_mlratio_final.RData")
  id <- findCommonID(f2g$pheno$MouseNum, rownames(islet.mlratio))
  f2g <- f2g[,id$first]
  islet.mlratio <- islet.mlratio[id$second,]
  f2g <- calc.genoprob(f2g, step=0.5, stepwidth="max", err=0.002, map.function="c-f")
  for(i in 1:2)
    f2g$pheno[,etraits[i]] <- islet.mlratio[,etraits[i]]
  out.rev <- scanone(f2g, method="hk", phe=etraits, chr="-un")
  out.rev <- replacemap(out.rev, oldgmap)

  save(out, out.rev, file=file)
}

source("colors.R")
bgcolor <- broman::brocolors("bg")
color <- broman::brocolors("crayons")[c("Cornflower", "Blush")]

pdf("../Figs/eqtl_lod_1.pdf", width=9, height=6.5, pointsize=12, onefile=TRUE)
par(fg="white", col="white", col.axis="white", col.lab=color[1], bg=bgcolor,
    mar=c(4.1,4.1,0.6,0.1))
plot(out, lod=1, col=color[1], ylab="LOD score")
text(xaxisloc.scanone(out, 1, 63.2)+20, 150, paste("probe", etraits[1], "(on chr 1)"), col=color[1], adj=c(0, 0.5))
points(xaxisloc.scanone(out, 1, 63.8), 5, col=color[1], pch=8)
dev.off()

pdf("../Figs/eqtl_lod_2.pdf", width=9, height=6.5, pointsize=12, onefile=TRUE)
par(fg="white", col="white", col.axis="white", col.lab=color[1], bg=bgcolor,
    mar=c(4.1,4.1,0.6,0.1))
plot(out, lod=1, col=color[1], ylab="LOD score")
text(xaxisloc.scanone(out, 1, 63.2)+20, 150, paste("probe", etraits[1], "(on chr 1)"), col=color[1], adj=c(0, 0.5))
points(xaxisloc.scanone(out, 1, 63.8), 5, col=color[1], pch=8)
plot(out, lod=2, col=color[2], add=TRUE)
text(xaxisloc.scanone(out, 6, 73.4)+20, 123, paste("probe", etraits[2], "(on chr 13)"), col=color[2], adj=c(0, 0.5))
points(xaxisloc.scanone(out, 13, 53.2), 5, col=color[2], pch=8)
dev.off()



pdf("../Figs/eqtl_lod_3.pdf", width=9, height=6.5, pointsize=12, onefile=TRUE)
par(fg="white", col="white", col.axis="white", col.lab=color[1], bg=bgcolor,
    mar=c(4.1,4.1,0.6,0.1))
plot(out.rev, lod=1, col=color[1], ylab="LOD score")
newcol <- colorRampPalette(c(color[1],"white"))(3)[2]
out[out[,1] != "1",3] <- NA
plot(out, lod=1, lty=2, col=newcol, add=TRUE)
text(xaxisloc.scanone(out, 1, 63.2)+20, 475, paste("probe", etraits[1], "(on chr 1)"), col=color[1], adj=c(0, 0.5))
points(xaxisloc.scanone(out, 1, 63.8), 13, col=color[1], pch=8)
plot(out.rev, lod=2, col=color[2], add=TRUE)
newcol <- colorRampPalette(c(color[2],"white"))(3)[2]
out[out[,1] != "6",4] <- NA
plot(out, lod=2, lty=2, col=newcol, add=TRUE)
text(xaxisloc.scanone(out, 6, 73.4)+20, 232, paste("probe", etraits[2], "(on chr 13)"), col=color[2], adj=c(0, 0.5))
points(xaxisloc.scanone(out, 13, 53.2), 13, col=color[2], pch=8)
dev.off()
