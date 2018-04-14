# image of genotypes + phenotypes

png("../Figs/data_fig.png", height=1000, width=1500, pointsize=28)

load("~/Projects/Attie/GoldStandard/FinalData/aligned_geno_with_pmap.RData")
load("~/Projects/Attie/GoldStandard/FinalData/lipomics_final_rev2.RData")
phe <- lipomics[,c("MouseNum","INSULIN (ng/ml) 10 wk"),drop=FALSE]

id <- lineup::findCommonID(f2g$pheno$MouseNum, phe$MouseNum)
f2g <- subset(f2g, ind=id$first)
f2g$pheno <- cbind(f2g$pheno, insulin=phe[id$second,2])

layout(cbind(1,2), width=c(1,0.22))

color <- broman::brocolors("crayons")[c("White", "Cornflower", "Dandelion", "Blush")]
par(mar=c(5.1,4.1,3.6,0.6))
names(f2g$geno)[20] <- ""
geno.image(f2g, col=color, main="", ylab="Mice", reorder=nphe(f2g))
u <- par("usr")
axis(side=3, at=u[2], labels="X", xpd=TRUE, line=-0.5)

y <- log10( f2g$pheno$insulin[order(f2g$pheno$insulin)] )
par(cex.lab=0.8, cex.axis=0.75)
grayplot(y, seq_along(y), xlab="log10 insulin", ylab="Mice", bgcolor="gray90",
         yaxs="i", ylim=c(0.5, length(y)+0.5), pch=16, col="slateblue",
         xaxs="i", cex=0.8, xlim=range(y)*c(0.98, 1.02))

dev.off()
