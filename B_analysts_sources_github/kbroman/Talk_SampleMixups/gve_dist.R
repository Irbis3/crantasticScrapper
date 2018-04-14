######################################################################
# distribution of proportion of mismatches
######################################################################
library(lineup)
source("colors.R")
bgcolor <- broman::brocolors("bg")

attach("~/Projects/Attie/GoldStandard/LiningUp/Data/genodist_t.RData")

pdf("../Figs/gve_dist.pdf", width=9, height=6.5, pointsize=12, onefile=TRUE)
par(fg="white", col="white", col.axis="white", col.lab="white", bg=bgcolor)
par(mfrow=c(2,1), mar=c(5.1,0.1,0.1,0.1))
pd <- pulldiag(totd)
hist(pd, breaks=seq(0, 1, len=201), ylab="", xlab="Proportion of mismatches", main="", yaxt="n")
rug(pd[pd>0.2], col=color[2], lwd=1.3)
u <- par("usr")
text(u[2]-diff(u[1:2])*0.05, mean(u[3:4]), "Self-self", col=color[1],
     adj=c(1,0.5), cex=1.5)
segments(0, u[3], 1, u[3], xpd=TRUE, lend=1, ljoin=1)

od <- omitdiag(totd)
hist(od, breaks=seq(0, 1, len=201), ylab="", xlab="Proportion of mismatches", main="", yaxt="n")
rug(od[od < 0.2], col=color[2], lwd=1.3)
u <- par("usr")
text(u[2]-diff(u[1:2])*0.05, mean(u[3:4]), "Self-nonself", col=color[1],
     adj=c(1,0.5), cex=1.5)
segments(0, u[3], 1, u[3], xpd=TRUE, lend=1, ljoin=1)
dev.off()



######################################################################
# scatterplots of distances
######################################################################
pd <- pulldiag(totd)
minrow <- apply(totd, 1, min, na.rm=TRUE)
whminrow <- apply(totd, 1, function(a, b) b[!is.na(a) & a==min(a, na.rm=TRUE)], colnames(totd))
nextbestrow <- apply(totd, 1, function(a) sort(a)[2])
selfrow <- minrow
selfrow[is.na(match(names(selfrow), names(pd)))] <- NA
selfrow[!is.na(match(names(selfrow), names(pd)))] <- pd
mincol <- apply(totd, 2, min, na.rm=TRUE)
whmincol <- apply(totd, 2, function(a,b) b[!is.na(a) & a==min(a, na.rm=TRUE)], rownames(totd))
nextbestcol <- apply(totd, 2, function(a) sort(a)[2])
selfcol <- mincol
selfcol[is.na(match(names(selfcol), names(pd)))] <- NA
selfcol[!is.na(match(names(selfcol), names(pd)))] <- pd

# final calls
attach("~/Projects/Attie/GoldStandard/LiningUp/Data/final_calls.RData")
final.calls <- sub("\\*", "", final.calls)
final.calls.rows <- final.calls[names(minrow),]
rn <- rownames(final.calls.rows)
thecall <- final.calls.rows[,"genotypes"]

notfound <- rn[c(grep("^maybe", thecall), grep("^not", thecall))]
okay <- rn[rn==thecall]
bad <- rn[rn != thecall]
bad <- bad[is.na(match(bad, notfound))]

pdf("../Figs/gve_dist_byrow.pdf", width=10, height=6.5, pointsize=12, onefile=TRUE)
par(fg="white", col="white", col.axis="white", col.lab="white", bg=bgcolor, pty="s")
par(mfrow=c(1,2), las=1)
plot(minrow, selfrow, xlab="minimum distance", ylab="self-self distance", type="n",
     ylim=c(0,0.84), xlim=c(0,0.84))
mtext(side=3, line=1, "Self vs best", col=color2[1], cex=1.5)
abline(0,1,lty=2)
points(minrow[okay], selfrow[okay], col=color[1], lwd=2)
points(minrow[notfound], selfrow[notfound], col=color[2], lwd=2)
points(minrow[bad], selfrow[bad], col=color[4], lwd=2)
text(0.16, 0.08, "Good", col=color[1], cex=1.3, adj=c(0, 0.5))
text(0.07, 0.84, "Fixable", col=color[4], cex=1.3, adj=c(0, 0.5))
text(0.4, 0.75, "Not found", col=color[2], cex=1.3, adj=c(0,0.5))

par(fg="white", col="white", col.axis="white", col.lab="white", bg=bgcolor)
plot(minrow, nextbestrow, xlab="minimum distance", ylab="2nd smallest distance", type="n",
     ylim=c(0,0.84), xlim=c(0,0.84))
mtext(side=3, line=1, "Next-best vs best", col=color2[1], cex=1.5)
abline(0,1,lty=2)
points(minrow[okay], nextbestrow[okay], col=color[1], lwd=2)
points(minrow[notfound], nextbestrow[notfound], col=color[2], lwd=2)
points(minrow[bad], nextbestrow[bad], col=color[4], lwd=2)
dev.off()

pdf("../Figs/gve_dist_byrow_left.pdf", width=9, height=6.5, pointsize=12, onefile=TRUE)
par(fg="white", col="white", col.axis="white", col.lab="white", bg=bgcolor, pty="s")
par(las=1)
plot(minrow, selfrow, xlab="minimum distance", ylab="self-self distance", type="n",
     ylim=c(0,0.84), xlim=c(0,0.84))
mtext(side=3, line=1, "Self vs best", col=color2[1], cex=1.5)
abline(0,1,lty=2)
points(minrow[okay], selfrow[okay], col=color[1], lwd=2)
points(minrow[notfound], selfrow[notfound], col=color[2], lwd=2)
points(minrow[bad], selfrow[bad], col=color[4], lwd=2)
text(0.16, 0.08, "Good", col=color[1], cex=1.3, adj=c(0, 0.5))
text(0.07, 0.84, "Fixable", col=color[4], cex=1.3, adj=c(0, 0.5))
text(0.275, 0.75, "Not found", col=color[2], cex=1.3, adj=c(0,0.5))
dev.off()

detach(2)
detach(2)
rm(final.calls)
