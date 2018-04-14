# illustration of recombinants and non-recombinants

pdf("../Figs/nonrec_illustrationA.pdf", height=3, width=5,
    pointsize=12)
par(mar=rep(0,4), bty="n")
plot(0,0,type="n", xlim=c(0,100), ylim=c(0,110),
     xaxt="n", yaxt="n", xlab="", ylab="")

xl <- c(0, 100)
y <- c(85, 70, 55, 35, 20, 5)
yd <- c(2.5, 1.5)

color <- brocolors("crayons")[c("Cornflower", "Blush")]

rect(xl[1], y[1]-yd[1]-yd[2], xl[2], y[1]-yd[1]+yd[2],
     border=NA, col=color[1])
rect(xl[1], y[1]+yd[1]-yd[2], xl[2], y[1]+yd[1]+yd[2],
     border=NA, col=color[1])

rect(xl[1], y[2]-yd[1]-yd[2], xl[2], y[2]-yd[1]+yd[2],
     border=NA, col=color[2])
rect(xl[1], y[2]+yd[1]-yd[2], xl[2], y[2]+yd[1]+yd[2],
     border=NA, col=color[1])

rect(xl[1], y[3]-yd[1]-yd[2], xl[2], y[3]-yd[1]+yd[2],
     border=NA, col=color[2])
rect(xl[1], y[3]+yd[1]-yd[2], xl[2], y[3]+yd[1]+yd[2],
     border=NA, col=color[2])

for(i in 1:3) { # lines above and below
  segments(xl[1], y[i]-yd[1]-yd[2], xl[2], y[i]-yd[1]-yd[2], lend=1,ljoin=1)
  segments(xl[1], y[i]-yd[1]+yd[2], xl[2], y[i]-yd[1]+yd[2], lend=1,ljoin=1)
  segments(xl[1], y[i]+yd[1]-yd[2], xl[2], y[i]+yd[1]-yd[2], lend=1,ljoin=1)
  segments(xl[1], y[i]+yd[1]+yd[2], xl[2], y[i]+yd[1]+yd[2], lend=1,ljoin=1)
}



x <- c(30, 70)
y <- c(95, 100, 105)
segments(x[1], y[2], x[2], y[2])
for(i in 1:2)
    segments(x[i], y[1], x[i], y[2])
text(mean(x), y[3], "QTL")


dev.off()


pdf("../Figs/nonrec_illustrationB.pdf", height=3, width=5,
    pointsize=12)
par(mar=rep(0,4), bty="n")
plot(0,0,type="n", xlim=c(0,100), ylim=c(0,110),
     xaxt="n", yaxt="n", xlab="", ylab="")

xl <- c(0, 100)
y <- c(85, 70, 55, 35, 20, 5)
yd <- c(2.5, 1.5)

color <- brocolors("crayons")[c("Cornflower", "Blush")]

rect(xl[1], y[1]-yd[1]-yd[2], xl[2], y[1]-yd[1]+yd[2],
     border=NA, col=color[1])
rect(xl[1], y[1]+yd[1]-yd[2], xl[2], y[1]+yd[1]+yd[2],
     border=NA, col=color[1])

rect(xl[1], y[2]-yd[1]-yd[2], xl[2], y[2]-yd[1]+yd[2],
     border=NA, col=color[2])
rect(xl[1], y[2]+yd[1]-yd[2], xl[2], y[2]+yd[1]+yd[2],
     border=NA, col=color[1])

rect(xl[1], y[3]-yd[1]-yd[2], xl[2], y[3]-yd[1]+yd[2],
     border=NA, col=color[2])
rect(xl[1], y[3]+yd[1]-yd[2], xl[2], y[3]+yd[1]+yd[2],
     border=NA, col=color[2])

for(i in 1:3) { # lines above and below
  segments(xl[1], y[i]-yd[1]-yd[2], xl[2], y[i]-yd[1]-yd[2], lend=1,ljoin=1)
  segments(xl[1], y[i]-yd[1]+yd[2], xl[2], y[i]-yd[1]+yd[2], lend=1,ljoin=1)
  segments(xl[1], y[i]+yd[1]-yd[2], xl[2], y[i]+yd[1]-yd[2], lend=1,ljoin=1)
  segments(xl[1], y[i]+yd[1]+yd[2], xl[2], y[i]+yd[1]+yd[2], lend=1,ljoin=1)
}


# rec
recl <- c(56.66303, 63.06189, 45.79075)
# first (red->blue,red), first(blue->red,blue), second(red,blue->red)
rect(xl[1], y[4]-yd[1]-yd[2], recl[1], y[4]-yd[1]+yd[2],
     border=NA, col=color[2])
rect(recl[1], y[4]-yd[1]-yd[2], xl[2], y[4]-yd[1]+yd[2],
     border=NA, col=color[1])
rect(xl[1], y[4]+yd[1]-yd[2], xl[2], y[4]+yd[1]+yd[2],
     border=NA, col=color[2])

rect(xl[1], y[5]-yd[1]-yd[2], recl[2], y[5]-yd[1]+yd[2],
     border=NA, col=color[1])
rect(recl[2], y[5]-yd[1]-yd[2], xl[2], y[5]-yd[1]+yd[2],
     border=NA, col=color[2])
rect(xl[1], y[5]+yd[1]-yd[2], xl[2], y[5]+yd[1]+yd[2],
     border=NA, col=color[1])

rect(xl[1], y[6]-yd[1]-yd[2], xl[2], y[6]-yd[1]+yd[2],
     border=NA, col=color[1])
rect(xl[1], y[6]+yd[1]-yd[2], recl[3], y[6]+yd[1]+yd[2],
     border=NA, col=color[1])
rect(recl[3], y[6]+yd[1]-yd[2], xl[2], y[6]+yd[1]+yd[2],
     border=NA, col=color[2])

for(i in 4:6) { # lines above and below
  segments(xl[1], y[i]-yd[1]-yd[2], xl[2], y[i]-yd[1]-yd[2], lend=1,ljoin=1)
  segments(xl[1], y[i]-yd[1]+yd[2], xl[2], y[i]-yd[1]+yd[2], lend=1,ljoin=1)
  segments(xl[1], y[i]+yd[1]-yd[2], xl[2], y[i]+yd[1]-yd[2], lend=1,ljoin=1)
  segments(xl[1], y[i]+yd[1]+yd[2], xl[2], y[i]+yd[1]+yd[2], lend=1,ljoin=1)
}

x <- c(30, 70)
y <- c(95, 100, 105)
segments(x[1], y[2], x[2], y[2])
for(i in 1:2)
    segments(x[i], y[1], x[i], y[2])
text(mean(x), y[3], "QTL")


dev.off()
