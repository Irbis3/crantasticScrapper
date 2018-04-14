library(qtlpvl)
library(qtl)
library(lineup)

folder <- "../TransBandsPaper/Data/"
para <- "lod_10.transcM_10.windowcM_10.count_50"
load(paste0(folder, "transbands.", para, "-sep.RData"))
load(paste0(folder, "test1vs2.", para, "-sep.RData"))
load(paste0(folder, "transbands.pvalue.nsimu_1000.RData"))
load(paste0(folder, "f2g.calc.RData"))
map <- pull.map(f2g)

toplot <- data.frame(tissue=c("islet", "islet", "kidney", "liver", "adipose"),
                     chr=c(6, 2, 13, 17, 10), stringsAsFactors=FALSE)

source("my_plottrans.R")

orange <- rgb(230, 159, 0, maxColorValue = 256)
green <- rgb(27, 159, 120, maxColorValue = 256)
blue <- rgb(123, 104, 238, maxColorValue = 256)
yellow <- rgb(255, 255, 0, maxColorValue = 256)
genecolor <- c(blue, orange, green, yellow)


for(i in 1:nrow(toplot)) {
    cat(i,"\n")

    pdf(paste0("../Figs/ldapca_", toplot$tissue[i], toplot$chr[i], ".pdf"), width=10, height=6.5)
    layout(rbind(c(1,2), c(3,3)), height=c(8,1))
    par(oma=c(0,0,2,0)+3)
    bn <- rownames(info.all)
    s <- bn[info.all$tissue==toplot$tissue[i] & info.all$chr==toplot$chr[i]]

    transband <- get(paste0("transband.", s))
    Y <- attr(transband, "Y")
    geno <- attr(transband, "geno")
    nonrecomb <- attr(transband, "nonrecomb")

    par(mar=c(3,3,2.6,2))
    my_plottrans.LDA(Y, geno, nonrecomb)
    title(main="LDA", col.main="firebrick", cex.main=2.1)
    my_plottrans.PCA(Y, geno, nonrecomb)
    title(main="PCA", col.main="firebrick", cex.main=2.1)

    par(bty="n", mar=c(0,3,2,2))
    plot(0,0,type="n",xaxt="n", yaxt="n", xlab="", ylab="",
         xlim=c(0, 100), xaxs="i", ylim=c(-1,1))
    x <- 5 + seq(0, by=18, len=4)
    xd <- 2
    x <- x-mean(x) + 40
    points(x, rep(0, 4), pch=21, bg=genecolor, cex=1.8)
    text(x+xd, rep(0, 4), c("BB", "BR", "RR", "Recombinant"), cex=1.8,
         adj = c(0, 0.5))

    dev.off()
}
