# illustration of QTL effects

pdf("../Figs/effect_illustrationA.pdf", height=3, width=5,
    pointsize=20)
par(mar=rep(0,4), bty="n")
plot(0,0,type="n", xlim=c(0,100), ylim=c(0,110),
     xaxt="n", yaxt="n", xlab="", ylab="")

x <- c(25, 65, 80)
xmid <- mean(x[c(1,3)])
y <- c(65, 80, 95)
yd <- 3

segments(0, y[1], 100, y[1])
for(i in 1:3) {
    segments(x[i], y[1]-yd, x[i], y[1]+yd)
}
text(x[1], y[1]-3*yd, expression(mu[BB]))
text(x[2], y[1]-3*yd, expression(mu[BR]))
text(x[3], y[1]-3*yd, expression(mu[RR]))

dev.off()



pdf("../Figs/effect_illustrationB.pdf", height=3, width=5,
    pointsize=20)
par(mar=rep(0,4), bty="n")
plot(0,0,type="n", xlim=c(0,100), ylim=c(0,110),
     xaxt="n", yaxt="n", xlab="", ylab="")

x <- c(25, 65, 80)
xmid <- mean(x[c(1,3)])
y <- c(65, 80, 95)
yd <- 3

segments(0, y[1], 100, y[1])
for(i in 1:3) {
    segments(x[i], y[1]-yd, x[i], y[1]+yd)
}
text(x[1], y[1]-3*yd, expression(mu[BB]))
text(x[2], y[1]-3*yd, expression(mu[BR]))
text(x[3], y[1]-3*yd, expression(mu[RR]))

segments(xmid, y[2], x[2], y[2])
for(xx in c(xmid, x[2]))
    segments(xx, y[2], xx, y[2]-yd)
segments(xmid, y[3], x[3], y[3])
for(xx in c(xmid, x[3]))
    segments(xx, y[3], xx, y[3]-yd)
yd <- 6
text(mean(c(xmid, x[2])), y[2]+yd, "d")
text(mean(c(xmid, x[3])), y[3]+yd, "a")

dev.off()
