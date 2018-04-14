library(broman)
bgcolor <- brocolors("bg")

color <- c("#7448C5", "#C167C3", "#FB7D71", "#AAE3A2")

x <- c(0, 35)
xw <- c(30, 35+2+8+2+13)
y <- 65
yw <- 10

pdf("../Figs/scale_fig1.pdf", height=6.5, width=10, pointsize=28)
par(fg="white",col="white",col.axis="white",col.lab="white",col.main="white",
    bg=bgcolor, mar=rep(0.1, 4), bty="n")

par(mar=rep(0, 4))
plot(0,0,type="n", bty="n", xlab="",ylab="", xaxt="n", yaxt="n",
     xlim=c(-1, 99), ylim=c(2, 67))

rect(x[1], y, x[1]+xw[1], y-yw, col=color[1], border=color[1], lend=1, ljoin=1)
text(x[1]+2, y-4, "genotypes", col=bgcolor, adj=c(0, 0))

rect(x[2], y, x[2]+xw[2], y-yw, col=color[2], border=color[2], lend=1, ljoin=1)
text(x[2]+2, y-4, "phenotypes", col=bgcolor, adj=c(0, 0))

dev.off()


pdf("../Figs/scale_fig2.pdf", height=6.5, width=10, pointsize=28)
par(fg="white",col="white",col.axis="white",col.lab="white",col.main="white",
    bg=bgcolor, mar=rep(0.1, 4), bty="n")

par(mar=rep(0, 4))
plot(0,0,type="n", bty="n", xlab="",ylab="", xaxt="n", yaxt="n",
     xlim=c(-1, 99), ylim=c(2, 67))

rect(x[1], y, x[1]+xw[1], y-yw, col=color[1], border=color[1], lend=1, ljoin=1)
text(x[1]+2, y-4, "genotypes", col=bgcolor, adj=c(0, 0))

rect(x[2], y, x[2]+xw[2], y-yw, col=color[2], border=color[2], lend=1, ljoin=1)
text(x[2]+2, y-4, "phenotypes", col=bgcolor, adj=c(0, 0))

rect(35-25, 65-25, 95-25, 35-25, col=color[3], border=color[3], lend=1, ljoin=1)
text(40-25, 60-25, "results", col=bgcolor, adj=c(0, 0))

dev.off()
