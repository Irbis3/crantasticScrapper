library(lineup)
attach("~/Projects/Attie/GoldStandard/LiningUp/Data/genodist_t.RData")

id <- findCommonID(rownames(totd), colnames(totd))
whr <- match(1:nrow(totd), id$first)
whc <- match(1:ncol(totd), id$second)
totdo <- matrix(ncol=ncol(totd), nrow=nrow(totd))
dimnames(totdo) <- list(c(rownames(totd)[id$first], rownames(totd)[is.na(whr)]),
                        c(colnames(totd)[id$second], colnames(totd)[is.na(whc)]))
for(i in 1:nrow(totdo))
  totdo[i,] <- totd[rownames(totdo)[i], colnames(totdo)]


source("colors.R")
bluepal <- colorRampPalette(c("blue","white"))
redpal <- colorRampPalette(c("white","red"))
col <- c(bluepal(100), redpal(412))
bgcolor <- broman::brocolors("bg")
bgcolorpng <- broman::brocolors("bg")


pdf("../Figs/distmat201.pdf", width=7.5, height=6.5, pointsize=12, onefile=TRUE)
par(fg="white", col="white", col.axis="white", col.lab="white", bg=bgcolor)
par(mfrow=c(2,1), mar=c(5.1,0.1,0.1,0.1))
par(las=1, mar=c(5.1,4.1,1.1,1.1))
layout(cbind(1,2), width=c(5,1))
x <- 201:300
image(x, x, totdo[x,x], col=col, xlab="DNA sample", ylab="mRNA sample",
      zlim=c(0,1))
rect(x-0.5, x-0.5, x+0.5, x+0.5, border="black", lwd=1, lend=1, ljoin=1)
u <- par("usr")
abline(h=u[3:4], v=u[1:2], col="white")
axis(side=1, at=201)
axis(side=2, at=201)

par(mar=c(12.1,3.1,8.1,1.1))
z <- seq(0, 1, length=512)
image(1, z, t(cbind(z)), zlim=c(0,1), col=col,
      xaxt="n", xlab="", ylab="")
u <- par("usr")
abline(h=u[3:4], v=u[1:2], col="white")
dev.off()


pdf("../Figs/distmat001.pdf", width=7.5, height=6.5, pointsize=12, onefile=TRUE)
par(fg="white", col="white", col.axis="white", col.lab="white", bg=bgcolor)
par(mfrow=c(2,1), mar=c(5.1,0.1,0.1,0.1))
par(las=1, mar=c(5.1,4.1,1.1,1.1))
layout(cbind(1,2), width=c(5,1))
x <- 1:100
image(x, x, totdo[x,x], col=col, xlab="DNA sample", ylab="mRNA sample",
      zlim=c(0,1))
rect(x-0.5, x-0.5, x+0.5, x+0.5, border="black", lwd=1, lend=1, ljoin=1)
u <- par("usr")
abline(h=u[3:4], v=u[1:2], col="white")
axis(side=1, at=1)
axis(side=2, at=1)

par(mar=c(12.1,3.1,8.1,1.1))
z <- seq(0, 1, length=512)
image(1, z, t(cbind(z)), zlim=c(0,1), col=col,
      xaxt="n", xlab="", ylab="")
u <- par("usr")
abline(h=u[3:4], v=u[1:2], col="white")
dev.off()


png("../Figs/distmatall.png", width=2250, height=1950, pointsize=48)
par(fg="white", col="white", col.axis="white", col.lab="white", bg=bgcolorpng)
par(mfrow=c(2,1), mar=c(5.1,0.1,0.1,0.1))
par(las=1, mar=c(5.1,4.1,1.1,1.1))
layout(cbind(1,2), width=c(5,1))
image(1:nrow(totdo), 1:ncol(totdo), totdo, col=col, xlab="DNA sample", ylab="mRNA sample",
      zlim=c(0,1))
u <- par("usr")
box(col="white")
abline(h=length(id$first)+0.4, v=length(id$first)+0.4, col=bgcolor, lwd=1)
axis(side=1, at=1)
axis(side=2, at=1)

par(mar=c(12.1,3.1,8.1,1.1))
z <- seq(0, 1, length=512)
image(1, z, t(cbind(z)), zlim=c(0,1), col=col,
      xaxt="n", xlab="", ylab="")
u <- par("usr")
abline(h=u[3:4], v=u[1:2], col="white")
dev.off()


pdf("../Figs/distmatdiag.pdf", width=9, height=6.5, pointsize=12, onefile=TRUE)
par(fg="white", col="white", col.axis="white", col.lab=color[1], bg=bgcolor)
par(mar=c(5.1,4.1,1.1,0.1), las=1)
plot(diag(totdo)[1:length(id$first)], xlab="Mouse", ylab="Proportion of mismatches",
     ylim=c(0,1), col=color[1], yaxs="i")
dev.off()
