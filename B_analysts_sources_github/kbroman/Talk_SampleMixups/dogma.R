######################################################################
# DNA -> phenotype
######################################################################

source("colors.R")


pdf("../Figs/dogma2.pdf", width=9, height=1.5, pointsize=24)
par(mar=rep(0.1, 4), fg="white", col="white", col.axis="white", col.lab=color[1],
    bg=bgcolor, bty="n")
plot(0, 0, type="n", xlab="", ylab="", xlim=c(0,100), ylim=c(0,100),
     xaxt="n", yaxt="n")

x <- seq(10, 90, length=4)
text(x, rep(60, 4), c("DNA", "mRNA", "protein", "   phenotype"))
text(x[1], 30, "(genotype)", col=color[1])

xm <- (x[-1]+x[-4])/2
xd <- diff(x[1:2])

arrows(xm-xd/6, 60, xm+xd/6, 60, len=0.1, lwd=2, col=color[2])

dev.off()

pdf("../Figs/dogma1.pdf", width=9, height=1.5, pointsize=24)
par(mar=rep(0.1, 4), fg="white", col="white", col.axis="white", col.lab=color[1],
    bg=bgcolor, bty="n")
plot(0, 0, type="n", xlab="", ylab="", xlim=c(0,100), ylim=c(0,100),
     xaxt="n", yaxt="n")

text(x[c(1,4)], rep(60, 2), c("DNA", "   phenotype"))
text(x[1], 30, "(genotype)", col=color[1])

xm <- (x[-1]+x[-4])/2
xd <- diff(x[1:2])

arrows(x[2], 60, x[3], 60, len=0.1, lwd=2, col=color[2])

dev.off()

