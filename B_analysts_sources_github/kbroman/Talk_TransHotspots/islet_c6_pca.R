source("color.R")
library(qtl)
library(broman)
library(lineup)
data.folder <- "../AttieData/"
data.folder.2 <- "../eQTLscans/"

########################################################################

load(paste0(data.folder, "input/aligned_geno_with_pmap.RData"))
load(paste0(data.folder, "f2g.calc.RData"))
load(paste0(data.folder, "islet_mlratio_final.RData"))
id <- findCommonID(f2g$pheno$MouseNum, rownames(islet.mlratio))
islet.mlratio <- islet.mlratio[id$second,]
f2g <- f2g[,id$first]

peakmarker <- "rs8262456"
peakpos <- find.markerpos(f2g, peakmarker)$pos

load(paste0(data.folder.2, "eqtl_results.RData"))
index <- which(probepos$chr != 6 & maxlod$islet[,6]>=100 & maxpos$islet[,6]>=peakpos-5 & maxpos$islet[,6]<=peakpos+5)
probes <- rownames(probepos)[index]

tmp <- islet.mlratio[,probes]
pc <- cmdscale(dist(tmp))
qtlg <- 4-as.numeric(cut(pc[,1], c(-Inf, -1, 1, Inf)))
names(qtlg) <- as.character(f2g$pheno$MouseNum)
qtlgn <- qtlg
qtlg <- as.character(qtlg)
qtlg[qtlg=="1"] <- "BB"
qtlg[qtlg=="2"] <- "BR"
qtlg[qtlg=="3"] <- "RR"
names(qtlg) <- f2g$pheno$MouseNum

##############################
# check genotypes in region and pick out the recombinants

g <- pull.geno(fill.geno(f2g[6,], method="maxmarginal", err=0.002, map.function="c-f"))
m <- pull.map(f2g)[[6]]
region <- which(m >= peakpos - 5 & m <= peakpos + 5)
rec <- which(apply(g[,region], 1, lenuniq) > 1)
nonrec <- which(apply(g[,region], 1, lenuniq) == 1)
all(apply(g[nonrec,region], 1, function(a) all(a == a[1]))) # non-rec are really non-rec
all(g[nonrec, region[1]] == match(qtlg[nonrec], c("BB", "BR", "RR"))) # genotypes match

names(rec) <- f2g$pheno$MouseNum[rec]
save(qtlg, rec, file="cache/qtl_geno.RData")

########################################################################
pdf("../Figs/islet_c6_pca.pdf", height=6.5, width=6.5)
par(las=1, mar=c(3.1, 3.8, 1.1, 1.1), pty="s")
x <- pc[,1]
y <- pc[,2]
px <- pretty(x, n=8)
py <- pretty(y, n=8)

qtlgn.nonrm <- qtlgn
qtlgn.nonrm[rec] <- NA
bg <- color[qtlgn.nonrm]
bg[is.na(bg)] <- "gray50"

par(col.lab="slateblue", cex.lab=1.1)
grayplot(x, y, pch=21, xlab="Principal component 1", ylab="Principal component 2",
         mgp.y=c(2.5, 0.2, 0), mgp.x=c(1.9, 0.2, 0), xat=px, yat=py,
         hlines=py, vlines=px,
         col="black", bg=bg)
points(x[rec],y[rec],col="black",bg="yellow",pch=21)

textcolor <- color
textcolor[2] <- "#af5500"
textcolor[1] <- "#4a3aad"

for(i in 1:3) {
  xx <- x[qtlgn==i]
  if(i==1) xx <- min(xx)-0.1
  else if(i==2) xx <- min(xx)-0.3
  else xx <- max(xx) + 0.3
  text(xx,
       median(y[qtlgn==i]), c("BB", "BR", "RR")[i],
       col=textcolor[i], cex=1.5)
}

legend("bottomright", pch=21, pt.bg="yellow", col="black", "recombinant",
       bg="gray80", cex=1.5)
dev.off()

######################################################################
pdf("../Figs/islet_c6_pca_norec.pdf", height=6.5, width=6.5)
par(las=1, mar=c(3.1, 3.8, 1.1, 1.1), pty="s")

par(col.lab="slateblue", cex.lab=1.1)
grayplot(x[-rec], y[-rec], pch=21, xlab="Principal component 1", ylab="Principal component 2",
         mgp.y=c(2.5, 0.2, 0), mgp.x=c(1.9, 0.2, 0), xat=px, yat=py,
         hlines=py, vlines=px,
         col="black", bg=bg[-rec])

for(i in 1:3) {
  xx <- x[qtlgn==i]
  if(i==1) xx <- min(xx)-0.1
  else if(i==2) xx <- min(xx)-0.3
  else xx <- max(xx) + 0.3
  text(xx,
       median(y[qtlgn==i]), c("BB", "BR", "RR")[i],
       col=textcolor[i], cex=1.5)
}
dev.off()
