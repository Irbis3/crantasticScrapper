suppressMessages(require(qtl))
suppressMessages(require(broman))
suppressMessages(require(lineup))
source("color.R")

data.folder <- "../AttieData/"
data.folder.2 <- "../eQTLscans/"

load(paste0(data.folder, "input/aligned_geno_with_pmap.RData"))

tissues <- c("islet", "adipose", "gastroc", "hypo", "kidney", "liver")

load(paste0(data.folder.2, "eqtl_results.RData"))
chr <- probepos$chr

lod.thr <- 5

localeqtl <- vector("list", 6)
transeqtl <- vector("list", 6)
for(i in 1:6) {
    lod <- maxlod[[i]][,6]
    pos <- maxpos[[i]][,6]
    localeqtl[[i]] <- list(pos=pos[lod > lod.thr & chr==6], lod=lod[lod > lod.thr & chr==6])
    transeqtl[[i]] <- list(pos=pos[lod > lod.thr & chr!=6], lod=lod[lod > lod.thr & chr!=6])
    names(localeqtl) <- names(transeqtl) <- names(maxlod)
}


ymax <- 185
pdf("../Figs/chr6_lod.pdf",w=9.5,h=6.5)
par(cex.lab=1.1, cex.main=1.8)
par(mfrow=c(2,3))
par(las=1, col.lab="slateblue", col.main="firebrick",
    mar=c(3.1, 3.1, 2.6, 1.1))

x <- seq(0, 150, by=20)
y <- seq(0, 175, by=25)

hcolor  <- "#1dacd6"
bgcolor <- "#b4674d"
pch <- 16

for(i in tissues) {
    grayplot(transeqtl[[i]]$pos, transeqtl[[i]]$lod,
             xlim=c(0, 100), xaxs="i",
             ylim=c(0, ymax), cex=0.5, pch=pch, col=hcolor,
             xlab="", ylab="LOD score", yaxs="i", main=i,
             hlines=y, vlines=x, mgp=c(1.6, 0.2, 0), xat=x, yat=y)
    abline(h=5, col="pink")
    points(localeqtl[[i]]$pos, localeqtl[[i]]$lod,
           cex=0.5, pch=pch, col=bgcolor)
    points(transeqtl[[i]]$pos, transeqtl[[i]]$lod,
           cex=0.5, pch=pch, col=hcolor)
    title(xlab="Position (cM)", mgp=c(1.6, 0, 0))
    rug(unlist(pull.map(f2g,chr=6)))
}

dev.off()
