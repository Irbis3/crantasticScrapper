######################################################################
# eQTL genotype vs expression
######################################################################

source("colors.R")
bgcolor <- broman::brocolors("bg")



pdf("../Figs/gve_scheme_1.pdf", width=9, height=6.5, pointsize=12, onefile=TRUE)
layout(cbind(c(1,1,2,2),c(4,3,3,5)))

par(fg="white", col="white", col.axis="white", col.lab="white", bg=bgcolor)
plot(0,0,type="n", xlab="", ylab="", xaxt="n", yaxt="n", xlim=c(0,100), ylim=c(0,100),
     xaxs="i", yaxs="i")
mtext(side=3,"expression traits", line=1, cex=1.5)
mtext(side=1,"transcripts", line=1, cex=1.3, col=color[1])
mtext(side=2,"mice", line=1, cex=1.3, col=color[1])

plot(0,0,type="n", xlab="", ylab="", xaxt="n", yaxt="n", xlim=c(0,100), ylim=c(0,100),
     xaxs="i", yaxs="i")
mtext(side=3,"observed eQTL genotypes", line=1, cex=1.5)
mtext(side=1,"eQTL", line=1, cex=1.3, col=color[1])
mtext(side=2,"mice", line=1, cex=1.3, col=color[1])
dev.off()

pdf("../Figs/gve_scheme_2.pdf", width=9, height=6.5, pointsize=12, onefile=TRUE)
layout(cbind(c(1,1,2,2),c(4,3,3,5)))

par(fg="white", col="white", col.axis="white", col.lab="white", bg=bgcolor)
plot(0,0,type="n", xlab="", ylab="", xaxt="n", yaxt="n", xlim=c(0,100), ylim=c(0,100),
     xaxs="i", yaxs="i")
mtext(side=3,"expression traits", line=1, cex=1.5)
mtext(side=1,"transcripts", line=1, cex=1.3, col=color[1])
mtext(side=2,"mice", line=1, cex=1.3, col=color[1])

plot(0,0,type="n", xlab="", ylab="", xaxt="n", yaxt="n", xlim=c(0,100), ylim=c(0,100),
     xaxs="i", yaxs="i")
mtext(side=3,"observed eQTL genotypes", line=1, cex=1.5)
mtext(side=1,"eQTL", line=1, cex=1.3, col=color[1])
mtext(side=2,"mice", line=1, cex=1.3, col=color[1])


plot(0,0,type="n", xlab="", ylab="", xaxt="n", yaxt="n", xlim=c(0,100), ylim=c(0,100),
     xaxs="i", yaxs="i")
mtext(side=3,"inferred eQTL genotypes", line=1, cex=1.5)
mtext(side=1,"eQTL", line=1, cex=1.3, col=color[1])
mtext(side=2,"mice", line=1, cex=1.3, col=color[1])
dev.off()

pdf("../Figs/gve_scheme_3.pdf", width=9, height=6.5, pointsize=12, onefile=TRUE)
layout(cbind(c(1,1,2,2),c(4,3,3,5)))

par(fg="white", col="white", col.axis="white", col.lab="white", bg=bgcolor)
plot(0,0,type="n", xlab="", ylab="", xaxt="n", yaxt="n", xlim=c(0,100), ylim=c(0,100),
     xaxs="i", yaxs="i")
mtext(side=3,"expression traits", line=1, cex=1.5)
mtext(side=1,"transcripts", line=1, cex=1.3, col=color[1])
mtext(side=2,"mice", line=1, cex=1.3, col=color[1])

plot(0,0,type="n", xlab="", ylab="", xaxt="n", yaxt="n", xlim=c(0,100), ylim=c(0,100),
     xaxs="i", yaxs="i")
mtext(side=3,"observed eQTL genotypes", line=1, cex=1.5)
mtext(side=1,"eQTL", line=1, cex=1.3, col=color[1])
mtext(side=2,"mice", line=1, cex=1.3, col=color[1])
rect(0,25,100,28,col=color[2],lend=1,ljoin=1)


plot(0,0,type="n", xlab="", ylab="", xaxt="n", yaxt="n", xlim=c(0,100), ylim=c(0,100),
     xaxs="i", yaxs="i")
mtext(side=3,"inferred eQTL genotypes", line=1, cex=1.5)
mtext(side=1,"eQTL", line=1, cex=1.3, col=color[1])
mtext(side=2,"mice", line=1, cex=1.3, col=color[1])
rect(0,25,100,28,col=color[2],lend=1,ljoin=1)
dev.off()

pdf("../Figs/gve_scheme_4.pdf", width=9, height=6.5, pointsize=12, onefile=TRUE)
layout(cbind(c(1,1,2,2),c(4,3,3,5)))

par(fg="white", col="white", col.axis="white", col.lab="white", bg=bgcolor)
plot(0,0,type="n", xlab="", ylab="", xaxt="n", yaxt="n", xlim=c(0,100), ylim=c(0,100),
     xaxs="i", yaxs="i")
mtext(side=3,"expression traits", line=1, cex=1.5)
mtext(side=1,"transcripts", line=1, cex=1.3, col=color[1])
mtext(side=2,"mice", line=1, cex=1.3, col=color[1])

plot(0,0,type="n", xlab="", ylab="", xaxt="n", yaxt="n", xlim=c(0,100), ylim=c(0,100),
     xaxs="i", yaxs="i")
mtext(side=3,"observed eQTL genotypes", line=1, cex=1.5)
mtext(side=1,"eQTL", line=1, cex=1.3, col=color[1])
mtext(side=2,"mice", line=1, cex=1.3, col=color[1])
rect(0,25,100,28,col=color[2],lend=1,ljoin=1)


plot(0,0,type="n", xlab="", ylab="", xaxt="n", yaxt="n", xlim=c(0,100), ylim=c(0,100),
     xaxs="i", yaxs="i")
mtext(side=3,"inferred eQTL genotypes", line=1, cex=1.5)
mtext(side=1,"eQTL", line=1, cex=1.3, col=color[1])
mtext(side=2,"mice", line=1, cex=1.3, col=color[1])
rect(0,65,100,68,col=color[4],lend=1,ljoin=1)
dev.off()
