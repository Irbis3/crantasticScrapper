library(broman)
bgcolor <- brocolors("bg")

color <- c("#7448C5", "#C167C3", "#FB7D71", "#AAE3A2")

x <- c(0, 35, 72, 82)
xw <- c(30, 35, 8, 13)
y <- seq(65, length=5, by=-12)
yw <- rep(10, 5)

for(i in 1:5) {

    pdf(paste0("../Figs/batches_fig", i, ".pdf"), height=6.5, width=10, pointsize=28)
    par(fg="white",col="white",col.axis="white",col.lab="white",col.main="white",
        bg=bgcolor, mar=rep(0.1, 4), bty="n")

    par(mar=rep(0, 4))
    plot(0,0,type="n", bty="n", xlab="",ylab="", xaxt="n", yaxt="n",
         xlim=c(-1, 99), ylim=c(2, 67))

    for(i in 1:i) {
        rect(x[1], y[i], x[1]+xw[1], y[i]-yw[i], col=color[1], border=color[1], lend=1, ljoin=1)
        rect(x[2], y[i], x[2]+xw[2], y[i]-yw[i], col=color[2], border=color[2], lend=1, ljoin=1)
    }
    text(x[1]+2, y[1]-4, "genotypes", col=bgcolor, adj=c(0, 0))
    text(x[2]+2, y[1]-4, "phenotypes", col=bgcolor, adj=c(0, 0))

    dev.off()
}


pdf("../Figs/batches_fig6.pdf", height=6.5, width=10, pointsize=28)
par(fg="white",col="white",col.axis="white",col.lab="white",col.main="white",
    bg=bgcolor, mar=rep(0.1, 4), bty="n")

par(mar=rep(0, 4))
plot(0,0,type="n", bty="n", xlab="",ylab="", xaxt="n", yaxt="n",
     xlim=c(-1, 99), ylim=c(2, 67))

for(i in 1:5) {
    rect(x[1], y[i], x[1]+xw[1], y[i]-yw[i], col=color[1], border=color[1], lend=1, ljoin=1)
    rect(x[2], y[i], x[2]+xw[2], y[i]-yw[i], col=color[2], border=color[2], lend=1, ljoin=1)
    rect(x[3], y[i], x[3]+xw[3], y[i]-yw[i], col=color[3], border=color[3], lend=1, ljoin=1)
}
text(x[1]+2, y[1]-4, "genotypes", col=bgcolor, adj=c(0, 0))
text(x[2]+2, y[1]-4, "phenotypes", col=bgcolor, adj=c(0, 0))

dev.off()



pdf("../Figs/batches_fig7.pdf", height=6.5, width=10, pointsize=28)
par(fg="white",col="white",col.axis="white",col.lab="white",col.main="white",
    bg=bgcolor, mar=rep(0.1, 4), bty="n")

par(mar=rep(0, 4))
plot(0,0,type="n", bty="n", xlab="",ylab="", xaxt="n", yaxt="n",
     xlim=c(-1, 99), ylim=c(2, 67))

for(i in 1:5) {
    rect(x[1], y[i], x[1]+xw[1], y[i]-yw[i], col=color[1], border=color[1], lend=1, ljoin=1)
    rect(x[2], y[i], x[2]+xw[2], y[i]-yw[i], col=color[2], border=color[2], lend=1, ljoin=1)
    rect(x[3], y[i], x[3]+xw[3], y[i]-yw[i], col=color[3], border=color[3], lend=1, ljoin=1)
    rect(x[4], y[i], x[4]+xw[4], y[i]-yw[i], col=color[4], border=color[4], lend=1, ljoin=1)
}
text(x[1]+2, y[1]-4, "genotypes", col=bgcolor, adj=c(0, 0))
text(x[2]+2, y[1]-4, "phenotypes", col=bgcolor, adj=c(0, 0))

dev.off()
