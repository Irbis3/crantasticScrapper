library(qtlpvl)
library(qtl)
library(lineup)

folder <- "../TransBandsPaper/Data/"
para <- "lod_10.transcM_10.windowcM_10.count_50"
load(paste0(folder, "transbands.", para, "-sep.RData"))
load(paste0(folder, "test1vs2.", para, "-sep.RData"))
load(paste0(folder, "transbands.pvalue.nsimu_1000.RData"))
load(paste0(folder, "f2g.calc.RData"))
map <- pull.map(f2g)

toplot <- data.frame(tissue=c("islet", "islet", "kidney", "liver", "adipose"),
                     chr=c(6, 2, 13, 17, 10), stringsAsFactors=FALSE)

source("my_plotGenetpattern.R")

for(i in 1:nrow(toplot)) {
    cat(i,"\n")

    pdf(paste0("../Figs/effects_", toplot$tissue[i], toplot$chr[i], ".pdf"), width=10, height=6.5)
    par(mfrow=c(1,2))
    par(oma=c(0,0,2,0)+3)
    bn <- rownames(info.all)
    s <- bn[info.all$tissue==toplot$tissue[i] & info.all$chr==toplot$chr[i]]

    transband <- get(paste0("transband.", s))
    out <- attr(transband, "out")
    chr <- attr(transband, "info")$chr

    par(mar=c(3,3,2,2),las=1)
    plotLODsign(maxPOS=out$pos, LODsign=sign(out$eff.a)*out$lod1, map=map[[chr]],
                main="signed LOD")
    par(las=1)
    my_plotGenetpattern(a=out$eff.a, d=out$eff.d, main="Inheritance Pattern",
                        xlab="Additive effect", ylab="Dominance effect")
    dev.off()
}
