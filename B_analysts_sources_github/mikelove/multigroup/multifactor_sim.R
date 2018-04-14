library(DESeq2)
set.seed(1)
dds <- makeExampleDESeqDataSet(m=5*4)
dds$group <- factor(rep(rep(1:2,each=5),2))
counts(dds)[1,] <- as.integer(rnbinom(5*4, mu=rep(2^(c(8,10,12,14)), each=5), size=1/.02))
design(dds) <- ~ group + condition
dds <- DESeq(dds)

intercept <- coef(dds)[1,1]
group <- rep(c(coef(dds)[1,2], coef(dds)[1,3]), 2)
groupSE <- rep(c(coef(dds,SE=TRUE)[1,2], coef(dds,SE=TRUE)[1,3]), 2)
cond <- rep(c(coef(dds)[1,4], coef(dds)[1,5]), each=2)
condSE <- rep(c(coef(dds,SE=TRUE)[1,4], coef(dds,SE=TRUE)[1,5]), each=2)


pdf(file="multifactor.pdf",width=5 ,height=5)
plotCounts(dds, "gene1", c("condition","group"), transform=TRUE, cex=.8, ylim=c(7,15), main="simulated multifactor")
abline(h=intercept, lwd=2, col="green", lty=2)
x <- 1:4 + .1
qn <- qnorm(.975)
segments(x, intercept, x, intercept+cond, col="purple", lwd=2)
points(x, intercept+cond, col="purple", lwd=2)
arrows(x, intercept+cond, x, intercept+cond+qn*condSE, col="purple", lwd=2, angle=90, length=.075)
arrows(x, intercept+cond, x, intercept+cond-qn*condSE, col="purple", lwd=2, angle=90, length=.075)
x <- 1:4 + .2
segments(x, intercept+cond, x, intercept+cond+group, col="red", lwd=2)
points(x, intercept+cond+group, col="red", lwd=2)
arrows(x, intercept+cond+group, x, intercept+cond+group+qn*groupSE, col="red", lwd=2, angle=90, length=.075)
arrows(x, intercept+cond+group, x, intercept+cond+group-qn*groupSE, col="red", lwd=2, angle=90, length=.075)
dev.off()
