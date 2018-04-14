######################################################################
# eQTL genotype vs expression
######################################################################

library(qtl)

source("colors.R")
bgcolor <- broman::brocolors("bg")

file <- "gve.RData"
if(file.exists(file)) {
  load(file)
} else {
  attach("~/Projects/Attie/GoldStandard/Genotypes/Study_and_clean/Data/clean_cross.RData")
  f2g <- replace.map(f2g, newmap)
  detach(2)
  attach("~/Projects/Attie/GoldStandard/LiningUp/Data/genodist_i.RData")
  attach("~/Projects/Attie/GoldStandard/LiningUp/Data/F2.mlratio.islet.RData")

  phenam <- sapply(attr(dgenovi, "y"), colnames)

  wh1 <- which(locallod.i == max(locallod.i)) # 499541 (1259) LOD = 165.5
  e1 <- names(wh1)
  chr1 <- strsplit(names(which(sapply(phenam, function(a,b) b %in% a, e1))),"@")[[1]][1]

  wh2 <- which(sapply(phenam, length)==2)
  chr2 <- strsplit(names(wh2), "@")[[1]][1]
  e2 <- phenam[[wh2]]

  wh3 <- which(sapply(phenam, length)==3)
  chr3 <- strsplit(names(wh3), "@")[[1]][1]
  e3 <- phenam[[wh3]][2:3]

  id <- findCommonID(colnames(islet.mlratio), f2g$pheno$MouseNum)

  f2g <- calc.genoprob(subset(f2g, c(chr1,chr2,chr3), id$second), map.function="c-f", err=0.002)
  y1 <- islet.mlratio[e1,id$first]
  y2 <- t(islet.mlratio[e2,id$first])
  y3 <- t(islet.mlratio[e3,id$first])
  out1 <- scanone(f2g, phe=y1, method="hk")
  out2a <- scanone(f2g, phe=y2[,1], method="hk")
  out2b <- scanone(f2g, phe=y2[,2], method="hk")
  out3a <- scanone(f2g, phe=y3[,1], method="hk")
  out3b <- scanone(f2g, phe=y3[,2], method="hk")
  marker1 <- rownames(max(out1))
  marker2 <- rownames(max(out2a))
  marker3 <- rownames(max(out3a))
  g <- pull.geno(fill.geno(f2g, err=0.002, map.function="c-f", method="argmax"))[,c(marker1, marker2,marker3)]

  save(y1, y2, y3, g, marker1, marker2, marker3, e1, e2, e3, chr1, chr2, chr3, file=file)
  detach(2)
  detach(2)
}


pdf("../Figs/gve1a.pdf", width=9, height=6.5, pointsize=12, onefile=TRUE)
par(mar=c(4.1,5.1,0.1,0.1), fg="white", col="white", col.axis="white", col.lab=color[1],
    bg=bgcolor, cex.axis=1.2, cex.lab=1.3, las=1)
plot(0, 0, type="n", xlab="", ylab=paste("expression  of ", e1), xaxt="n",
     xlim=c(0.75,3.25), ylim=range(y1))
set.seed(47500621)
u <- runif(length(y1), -0.1, 0.1)
points(g[,1]+u, y1, lwd=2, col=color[1])
axis(side=1, at=1:3, c("BB", "BR", "RR"))
title(xlab=paste("Genotype  at ", marker1))
dev.off()

pdf("../Figs/gve1a_nqrank.pdf", width=10, height=6, pointsize=12, onefile=TRUE)
par(mfrow=c(1,2))
par(mar=c(4.1,5.1,2.6,1.6), fg="white", col="white", col.axis="white", col.lab=color[1],
    bg=bgcolor, cex.axis=1.2, cex.lab=1.3, las=1, col.main="#FEEB96", cex.main=1.7)
plot(0, 0, type="n", xlab="", ylab=paste("expression  of ", e1), xaxt="n",
     xlim=c(0.75,3.25), ylim=range(y1), main="Untransformed")
set.seed(47500621)
u <- runif(length(y1), -0.1, 0.1)
points(g[,1]+u, y1, lwd=2, col=color[1])
axis(side=1, at=1:3, c("BB", "BR", "RR"))
title(xlab=paste("Genotype  at ", marker1))


par(mar=c(4.1,6.1,2.6,0.6), fg="white", col="white", col.axis="white", col.lab=color[1],
    bg=bgcolor, cex.axis=1.2, cex.lab=1.3, las=1)
y1nqr <- nqrank(y1)
plot(0, 0, type="n", xlab="", ylab=paste("transformed expression  of ", e1), xaxt="n",
     xlim=c(0.75,3.25), ylim=range(y1nqr), main="Transformed")
set.seed(47500621)
u <- runif(length(y1nqr), -0.1, 0.1)
points(g[,1]+u, y1nqr, lwd=2, col=color[1])
axis(side=1, at=1:3, c("BB", "BR", "RR"))
title(xlab=paste("Genotype  at ", marker1))
dev.off()

load("~/Projects/Attie/GoldStandard/Genotypes/Study_and_clean/Data/sex_swaps.RData")
sexswaps <- c(notfemale, notmale)
m <- match(sexswaps, names(y1))
m <- m[!is.na(m)]

pdf("../Figs/gve1b.pdf", width=9, height=6.5, pointsize=12, onefile=TRUE)
par(mar=c(4.1,5.1,0.1,0.1), fg="white", col="white", col.axis="white", col.lab=color[1],
    bg=bgcolor, cex.axis=1.2, cex.lab=1.3, las=1)
plot(0, 0, type="n", xlab="", ylab=paste("expression  of ", e1), xaxt="n",
     xlim=c(0.75,3.25), ylim=range(y1), las=1)
points(g[,1]+u, y1, lwd=2, col="gray40")
points((g[,1]+u)[m], y1[m], bg=color[2], col="white", lwd=1, pch=21)
axis(side=1, at=1:3, c("BB", "BR", "RR"))
title(xlab=paste("Genotype  at ", marker1))
dev.off()


pdf("../Figs/gve1c.pdf", width=9, height=6.5, pointsize=12, onefile=TRUE)
par(mar=c(4.1,5.1,0.1,0.1), fg="white", col="white", col.axis="white", col.lab=color[1],
    bg=bgcolor, cex.axis=1.2, cex.lab=1.3, las=1)
plot(0, 0, type="n", xlab="", ylab=paste("expression  of ", e1), xaxt="n",
     xlim=c(0.75,3.25), ylim=range(y1))
require(class)
g1i <-  knn(cbind(y1), cbind(y1), g[,1], k=40, l=33)
points(g[,1]+u, y1, lwd=2, col=color[c(1,4,2)][g1i])
points((g[,1]+u)[is.na(g1i)], y1[is.na(g1i)], lwd=2, col=color[6])
axis(side=1, at=1:3, c("BB", "BR", "RR"))
title(xlab=paste("Genotype  at ", marker1))
dev.off()


pdf("../Figs/gve2a.pdf", width=9, height=6.5, pointsize=12, onefile=TRUE)
par(mar=c(4.1,5.1,0.1,0.1), fg="white", col="white", col.axis="white", col.lab=color[1],
    bg=bgcolor, cex.axis=1.2, cex.lab=1.3, las=1)
plot(y2, col=color[c(2,4,1)][g[,2]], lwd=2,
     xlab=paste("expression  of ", e2[1]),
     ylab=paste("expression  of ", e2[2]))
text(-0.15, -1.0, "BB", col=color[1], cex=1.5)
text(0.20, -0.25, "BR", col=color[4], cex=1.5)
text(0.4, 0.45, "RR", col=color[2], cex=1.5)
u <- par("usr")
text(u[1]+diff(u[1:2])*0.06/2, u[4]-diff(u[3:4])*0.09/2,
     paste("Genotype  at ", marker2), cex=1.3, adj=c(0,0.5))
dev.off()

pdf("../Figs/gve2a_nqrank.pdf", width=9, height=6.5, pointsize=12, onefile=TRUE)
par(mar=c(4.1,5.1,0.1,0.1), fg="white", col="white", col.axis="white", col.lab=color[1],
    bg=bgcolor, cex.axis=1.2, cex.lab=1.3, las=1)
y2nqr <- apply(y2, 2, nqrank)
plot(y2nqr, col=color[c(2,4,1)][g[,2]], lwd=2,
     xlab=paste("expression  of ", e2[1]),
     ylab=paste("expression  of ", e2[2]))
u <- par("usr")
text(u[1]+diff(u[1:2])*0.06/2, u[4]-diff(u[3:4])*0.09/2,
     paste("Genotype  at ", marker2), cex=1.3, adj=c(0,0.5))
dev.off()

pdf("../Figs/gve2b.pdf", width=9, height=6.5, pointsize=12, onefile=TRUE)
par(mar=c(4.1,5.1,0.1,0.1), fg="white", col="white", col.axis="white", col.lab=color[1],
    bg=bgcolor, cex.axis=1.2, cex.lab=1.3, las=1)
plot(y2, col="gray40", lwd=2,
     xlab=paste("expression  of ", e2[1]),
     ylab=paste("expression  of ", e2[2]))
points(y2[m,], col="white", bg=color[c(2,4,1)][g[m,2]], lwd=1, pch=21)
text(-0.15, -1.0, "BB", col=color[1], cex=1.5)
text(0.20, -0.25, "BR", col=color[4], cex=1.5)
text(0.4, 0.45, "RR", col=color[2], cex=1.5)
u <- par("usr")
text(u[1]+diff(u[1:2])*0.06/2, u[4]-diff(u[3:4])*0.09/2,
     paste("Genotype  at ", marker2), cex=1.3, adj=c(0,0.5))
dev.off()

pdf("../Figs/gve3a.pdf", width=9, height=6.5, pointsize=12, onefile=TRUE)
par(mar=c(4.1,5.1,0.1,0.1), fg="white", col="white", col.axis="white", col.lab=color[1],
    bg=bgcolor, cex.axis=1.2, cex.lab=1.3, las=1)
plot(y3, col=color[c(2,4,1)][g[,3]], lwd=2,
     xlab=paste("expression  of ", e3[1]),
     ylab=paste("expression  of ", e3[2]))
text(-0.9, -0.05, "BB", col=color[1], cex=1.5)
text(-0.25, 0.25, "BR", col=color[4], cex=1.5)
text(0.05, 0.4, "RR", col=color[2], cex=1.5)
u <- par("usr")
text(u[1]+diff(u[1:2])*0.06/2, u[4]-diff(u[3:4])*0.09/2,
     paste("Genotype  at ", marker3), cex=1.3, adj=c(0,0.5))
dev.off()

pdf("../Figs/gve3a_nqrank.pdf", width=9, height=6.5, pointsize=12, onefile=TRUE)
par(mar=c(4.1,5.1,0.1,0.1), fg="white", col="white", col.axis="white", col.lab=color[1],
    bg=bgcolor, cex.axis=1.2, cex.lab=1.3, las=1)
ur <- par("usr")
y3nqr <- apply(y3, 2, nqrank)
plot(y3nqr, col=color[c(2,4,1)][g[,3]], lwd=2,
     xlab=paste("expression  of ", e3[1]),
     ylab=paste("expression  of ", e3[2]))
u <- par("usr")
text(u[1]+diff(u[1:2])*0.06/2, u[4]-diff(u[3:4])*0.09/2,
     paste("Genotype  at ", marker3), cex=1.3, adj=c(0,0.5))
dev.off()

pdf("../Figs/gve3b.pdf", width=9, height=6.5, pointsize=12, onefile=TRUE)
par(mar=c(4.1,5.1,0.1,0.1), fg="white", col="white", col.axis="white", col.lab=color[1],
    bg=bgcolor, cex.axis=1.2, cex.lab=1.3, las=1)
plot(y3, col="gray40", lwd=2,
     xlab=paste("expression  of ", e3[1]),
     ylab=paste("expression  of ", e3[2]))
points(y3[m,], col="white", bg=color[c(2,4,1)][g[m,3]], lwd=1, pch=21)
text(-0.9, -0.05, "BB", col=color[1], cex=1.5)
text(-0.25, 0.25, "BR", col=color[4], cex=1.5)
text(0.05, 0.4, "RR", col=color[2], cex=1.5)
u <- par("usr")
text(u[1]+diff(u[1:2])*0.06/2, u[4]-diff(u[3:4])*0.09/2,
     paste("Genotype  at ", marker3), cex=1.3, adj=c(0,0.5))
dev.off()
