# scan1 figure

source("colors.R")

library(qtl2)
file <- "_cache/rqtl2_results.RData"
if(file.exists(file)) {
    load(file)
} else {
    dir <- "~/Projects/AttieDO"
    apr <- readRDS(file.path(dir, "CalcGenoProb/attieDO_aprobs.rds"))
    k <- readRDS(file.path(dir, "CalcGenoProb/kinship.rds"))
    ka <- readRDS(file.path(dir, "CalcGenoProb/kinship_allchr.rds"))
    load(file.path(dir, "DerivedData/pheno_clin.RData"))
    sex <- (pheno_clin$sex=="M")+0
    names(sex) <- paste0("DO-", rownames(pheno_clin))
    phe <- pheno_clin[,sapply(pheno_clin, is.numeric)]
    rownames(phe) <- paste0("DO-", rownames(phe))
    wts <- phe[,grep("^weight_\\d+wk$", names(phe))]
    wts <- as.matrix(wts)

    pmap <- readRDS(file.path(dir, "CalcGenoProb/attieDO_pmap.rds"))

    out <- scan1(apr, wts[,12], addcovar=sex, cores=0)
    out_lmm <- scan1(apr[,17], wts[,12], ka, addcovar=sex, cores=0)
    out_loco <- scan1(apr[,17], wts[,12], k[17], addcovar=sex, cores=0)

    eff <- scan1coef(apr[,17], wts[,12,drop=FALSE], k[17], addcovar=sex)
    blup <- scan1blup(apr[,17], wts[,12,drop=FALSE], k[17], addcovar=sex)


    cl <- class(eff)
    eff[,1:8] <- eff[,1:8] - rowMeans(eff[,1:8])
    blup[,1:8] <- blup[,1:8] - rowMeans(blup[,1:8])
    class(eff) <- class(blup) <- cl

    # snp asso
    li <- lod_int(out_loco, pmap[17])
    library(RSQLite)
    db <- dbConnect(SQLite(), "~/Projects/AttieDO/DerivedData/ccfoundersnps.sqlite")
    snpinfo <- dbGetQuery(db, paste0("SELECT * FROM snps WHERE chr=='17' AND pos_Mbp >= ",
                          li[1], " AND pos_Mbp <= ", li[3]))
    dbDisconnect(db)
    snpinfo <- index_snps(pmap[17], snpinfo)
    snp_pr <- genoprob_to_snpprob(apr[,17], snpinfo)

    out_snps <- scan1(snp_pr, wts[,12], k[17], addcovar=sex, cores=0)

    db <- dbConnect(SQLite(), "~/Projects/AttieDO/GeneLoc/mgi_genes.sqlite")
    genes <- dbGetQuery(db, paste0("SELECT * FROM mgi_genes WHERE chr=='17' AND source=='MGI' AND ",
                                   "bioType == 'protein coding gene' AND ",
                                   "((start >= ", li[1]*1e6, " AND stop <= ", li[3]*1e6, ") OR ",
                                   "(start >= ", li[1]*1e6, " AND start <= ", li[3]*1e6, ") OR ",
                                   "(stop >= ", li[1]*1e6, " AND stop <= ", li[3]*1e6, ") OR ",
                                   "(start <= ", li[1]*1e6, " AND stop >= ", li[3]*1e6, "))"))
    dbDisconnect(db)

    save(out, out_lmm, out_loco, pmap, eff, blup, out_snps, snpinfo, genes, file=file)
}

pdf("../Figs/rqtl2_scan.pdf", height=7, width=10, pointsize=16)
par(mar=c(3.6, 3.1, 0.6, 0.6),las=1,fg="white",col="white",col.axis="white",col.lab="lightblue",
    bg=bgcolor,bty="n")
yli <- c(0, 9)
plot(out, pmap, ylim=yli, mgp=c(1.6, 0.3, 0))
dev.off()

lodcolors <- c("slateblue", "violetred", broman::brocolors("web")["green"])
pdf("../Figs/rqtl2_scan17.pdf", height=7, width=10, pointsize=16)
par(mar=c(3.6, 3.1, 0.6, 0.6),las=1,fg="white",col="white",col.axis="white",col.lab="lightblue",
    bg=bgcolor,bty="n")
plot(out_lmm, pmap[17], col=lodcolors[2], ylim=yli, xlab="Chr 17 position (Mbp)",
     mgp=c(1.6,0.3,0))
plot(out_loco, pmap[17], col=lodcolors[3], add=TRUE)
plot(out, pmap, chr=17, add=TRUE, col=lodcolors[1])
legend("topright", c("HK", "LMM", "LMM loco"), col=lodcolors,
       lwd=2, bg="gray90", text.col="black", box.col="black")
dev.off()

pdf("../Figs/rqtl2_coef.pdf", height=7, width=10, pointsize=16)
par(mar=c(3.6, 5.1, 2.3, 0.1),las=1,fg="white",col="white",col.axis="white",col.lab="lightblue",
    bg=bgcolor,bty="n")
layout(rbind(1,2), height=c(2,1))
par(mar=c(0.1, 3.1, 0.6, 0.6))
plot_coefCC(eff, pmap[17], xaxt="n", mgp=c(1.6,0.3,0), ylim=c(-4.4, 4.4))
par(mar=c(3.1, 3.1, 0.1, 0.6))
plot(out_loco, pmap[17], xlab="Chr 17 position (Mbp)", mgp=c(1.6,0.3,0))
dev.off()

pdf("../Figs/rqtl2_blup.pdf", height=7, width=10, pointsize=16)
par(mar=c(3.6, 5.1, 2.3, 0.1),las=1,fg="white",col="white",col.axis="white",col.lab="lightblue",
    bg=bgcolor,bty="n")
layout(rbind(1,2), height=c(2,1))
par(mar=c(0.1, 3.1, 0.6, 0.6))
plot_coefCC(blup, pmap[17], xaxt="n", mgp=c(1.6,0.3,0), ylim=c(-4.4, 4.4))
legend("topleft", names(CCcolors)[1:4], col=CCcolors[1:4], lwd=2,
       bg="gray90", text.col="black", box.col="black", text.width=6)
legend("bottomleft", names(CCcolors)[5:8], col=CCcolors[5:8], lwd=2,
       bg="gray90", text.col="black", box.col="black", text.width=6)
par(mar=c(3.1, 3.1, 0.1, 0.6))
plot(out_loco, pmap[17], xlab="Chr 17 position (Mbp)", mgp=c(1.6,0.3,0))
dev.off()


pdf("../Figs/rqtl2_snpasso.pdf", height=7, width=10, pointsize=16)
par(mar=c(3.6, 5.1, 2.3, 0.1),las=1,fg="white",col="white",col.axis="white",col.lab="lightblue",
    bg=bgcolor,bty="n")
layout(rbind(1,2), height=c(1,1))
par(mar=c(0.1, 3.1, 0.6, 0.6))
xli <- c(31.35, 35.12)
plot(out_snps, snpinfo, xaxt="n", mgp=c(1.6,0.3,0), xlim=xli, drop=1.5)
par(mar=c(3.1, 3.1, 0.1, 0.6))
plot_genes(genes, xlim=xli, mgp=c(1.6,0.3,0), xlab="Chr 17 position (Mbp)",
           colors=broman::brocolors("web")[c(2,4,5,6,9,10,11,12,13,14,15)])
dev.off()
