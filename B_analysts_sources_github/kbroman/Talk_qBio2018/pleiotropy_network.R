library(broman)
bgcolor <- brocolors("bg")
iArrows <- igraph:::igraph.Arrows

pdf("../Figs/pleiotropy_network.pdf", height=6, width=10, pointsize=32)
par(fg="white",col="white",col.axis="white",col.lab="white",col.main="white",
    bg=bgcolor, mar=rep(0.1, 4), bty="n")

plot(0,0, xaxt="n", yaxt="n", xlab="", ylab="", type="n",
     xlim=c(-16-2/3, 150), ylim=c(0, 100), xaxs="i", yaxs="i")
x <- c(25, 75)
y <- seq(10, 90, len=5)
text(x[1], y[2], expression(Q[2]))
text(x[1], y[4], expression(Q[1]))

text(x[2], y[1], expression(Y[3]))
text(x[2], y[3], expression(Y[2]))
text(x[2], y[5], expression(Y[1]))

arrowcol <- brocolors("crayons")["Cerulean"]
xd <- 8
yd <- 3.5
arrowlwd <- 5
arrowlen <- 0.3
arrows(x[1]+xd, y[2]-yd, x[2]-xd, y[1]+yd, lwd=arrowlwd,
      col=arrowcol, len=arrowlen)
arrows(x[1]+xd, y[2]+yd, x[2]-xd, y[3]-yd, lwd=arrowlwd,
      col=arrowcol, len=arrowlen)
arrows(x[1]+xd, y[4]-yd, x[2]-xd, y[3]+yd, lwd=arrowlwd,
      col=arrowcol, len=arrowlen)
arrows(x[1]+xd, y[4]+yd, x[2]-xd, y[5]-yd, lwd=arrowlwd,
      col=arrowcol, len=arrowlen)

arrowcol <- brocolors("crayons")["Tickle Me Pink"]
yd2 <- 4
xd2 <- 8
iArrows(x[1]-xd2, y[2]+yd2, x[1]-xd2, y[4]-yd2,
       h.lwd=arrowlwd, sh.lwd=arrowlwd,  sh.col=arrowcol,
       curve=1, width=arrowlwd, size=0.3, code=0)
# code=3 makes it double-headed
# code=0 makes no heads

# adding the heads "manually"
xd3 <- 0.05
yd3 <- 0.05
arrows(x[1]-xd2-xd3, y[4]-yd2-yd3, x[1]-xd2, y[4]-yd2,
       lwd=arrowlwd, col=arrowcol, len=arrowlen)
arrows(x[1]-xd2-xd3, y[2]+yd2+yd3, x[1]-xd2, y[2]+yd2,
       lwd=arrowlwd, col=arrowcol, len=arrowlen)

dev.off()
