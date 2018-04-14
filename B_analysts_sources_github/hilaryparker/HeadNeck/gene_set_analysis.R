setwd("/home/bst/student/hiparker/HeadNeck")
library("ProjectTemplate")
load.project()


####################################
# gene set analysis               ##
####################################

# getting things set up #
HPV <- as.factor(info.chung$HPV.Stat)
HPVmod <- model.matrix(~HPV)

# note that this function only works for HPV outcome right now #

# negative genes as a control #
# control genes on the array #
# probe names have affx #
# grep('aff',row.names(frma.chung),value=T,ignore.case=T) #
# part of general workflow! #


get.res <- function(dat,mod){
	
	#differential expression analysis#
	fit <- lmFit(dat,mod)
	fit <- eBayes(fit)
	
	#get gene names/symbols#
	fit$genes$ENTREZ <- unlist(mget(fit$genes$ID, hgu133plus2ENTREZID))
	fit$genes$SYMBOL <- unlist(mget(fit$genes$ID, hgu133plus2SYMBOL))

	res <- topTable(fit, number=Inf, coef='HPVPos', sort.by='p')
	res <- res[!duplicated(res[,'ENTREZ']),]
	res <- res[which(!is.na(res$ENTREZ)),]
	
	res.t <- res$t
	names(res.t) <- res$ENTREZ
	
	out<-list(res=res,res.t=res.t)

	return(out)
}

#out should be list -- first element is topTable, second element is t stats#
get.tab <- function(out,geneset){

	tmp <- lapply(geneset,intersect,out$res$ENTREZ)
	tmp <- tmp[sapply(tmp,length)>5]
	tmp <- tmp[sapply(tmp,length)<100]

	gsUp <- gsDown <- rep(NA,length(tmp))
	names(gsUp) <- names(gsDown) <- names(tmp)

	for (g in names(tmp)) {
		gsUp[g] <- geneSetTest(index=tmp[[g]],
							   statistics=out$res.t,
							   alternative='up')
		gsDown[g] <- geneSetTest(index=tmp[[g]],
						         statistics=out$res.t,
								 alternative='down')
	}

	gsUp <- p.adjust(gsUp,method='BH')
	gsDown <- p.adjust(gsDown, method='BH')


	gsTable <- data.frame(names(gsUp),
                      p.adjust(gsUp,method='BH'),
                      p.adjust(gsDown,method='BH'))
 
	colnames(gsTable) <- c('GeneSet', 'p.up.in.HPVPos',
                       'p.down.in.HPVPos')

	out<-list(reduced_geneset=tmp,gsTable=gsTable)
	return(out)
}

# fit for each of the four scenarios #

# no batch correction
frma.res <- get.res(frma.chung,HPVmod)
frma.tab <- get.tab(frma.res,c2Sets)

# combat correction
combat.frma.res <- get.res(combat.frma.chung,HPVmod)
combat.frma.tab <- get.tab(combat.frma.res,c2Sets)

# sva correction
sva.frma.res <- get.res(sva.frma.chung,HPVmod)
sva.frma.tab <- get.tab(sva.frma.res,c2Sets)

# combat and sva correction
sva.combat.frma.res <- get.res(sva.combat.frma.chung,HPVmod)
sva.combat.frma.tab <- get.tab(sva.combat.frma.res,c2Sets)



# geneset with targets that are most differentially expressed #
# for each of the combinations #

# no correction
r12<-names(frma.tab$reduced_geneset[which.min(frma.tab$gsTable$p.down.in.HPVPos)])
#[1] "REACTOME_EXTRACELLULAR_MATRIX_ORGANIZATION"
r11<-names(frma.tab$reduced_geneset[which.min(frma.tab$gsTable$p.up.in.HPVPos)])
#[1] "PYEON_HPV_POSITIVE_TUMORS_UP"

# combat correction
r22<-names(combat.frma.tab$reduced_geneset[which.min(combat.frma.tab$gsTable$p.down.in.HPVPos)])
#[1] "CROMER_TUMORIGENESIS_UP"
r21<-names(combat.frma.tab$reduced_geneset[which.min(combat.frma.tab$gsTable$p.up.in.HPVPos)])
#[1] "PYEON_HPV_POSITIVE_TUMORS_UP"

# sva correction
r32<-names(sva.frma.tab$reduced_geneset[which.min(sva.frma.tab$gsTable$p.down.in.HPVPos)])
#[1] "LIANG_SILENCED_BY_METHYLATION_2"
r31<-names(sva.frma.tab$reduced_geneset[which.min(sva.frma.tab$gsTable$p.up.in.HPVPos)])
#[1] "SLEBOS_HEAD_AND_NECK_CANCER_WITH_HPV_UP"

# combat + sva correction
r42<-names(sva.combat.frma.tab$reduced_geneset[which.min(sva.combat.frma.tab$gsTable$p.down.in.HPVPos)])
#[1] "BROWNE_INTERFERON_RESPONSIVE_GENES"
r41<-names(sva.combat.frma.tab$reduced_geneset[which.min(sva.combat.frma.tab$gsTable$p.up.in.HPVPos)])
#[1] "SLEBOS_HEAD_AND_NECK_CANCER_WITH_HPV_UP"

c1<-c(r11,r21,r31,r41)
c2<-c(r12,r22,r32,r42)
tabgenesets<-cbind(c1,c2)
colnames(tabgenesets)<-c("Up in HPV Positive","Down in HPV Positive")
rownames(tabgenesets)<-c("No Correction","ComBat Only","SVA Only","ComBat and SVA")

# barcode plots from gene sets identified above #

barcodeplot(statistics=frma.res$res.t, index=c2Sets[["SLEBOS_HEAD_AND_NECK_CANCER_WITH_HPV_UP"]])
barcodeplot(statistics=combat.frma.res$res.t, index=c2Sets[["SLEBOS_HEAD_AND_NECK_CANCER_WITH_HPV_UP"]])
barcodeplot(statistics=sva.frma.res$res.t, index=c2Sets[["SLEBOS_HEAD_AND_NECK_CANCER_WITH_HPV_UP"]])
barcodeplot(statistics=sva.combat.frma.res$res.t, index=c2Sets[["SLEBOS_HEAD_AND_NECK_CANCER_WITH_HPV_UP"]])


## maybe look up all the HPV genesets and see what is going on ##
setwd('./graphs')
pdf(file="test.pdf")
barcodeplot(statistics=frma.res$res.t, index=c2Sets[['RICKMAN_HEAD_AND_NECK_CANCER_E']])
barcodeplot(statistics=combat.frma.res$res.t, index=c2Sets[['RICKMAN_HEAD_AND_NECK_CANCER_E']])
barcodeplot(statistics=sva.frma.res$res.t, index=c2Sets[['RICKMAN_HEAD_AND_NECK_CANCER_E']])
barcodeplot(statistics=sva.combat.frma.res$res.t, index=c2Sets[['RICKMAN_HEAD_AND_NECK_CANCER_E']])
dev.off()

pdf(file="pyeon.pdf")
barcodeplot(statistics=frma.res$res.t, index=c2Sets[["PYEON_HPV_POSITIVE_TUMORS_UP"]])
barcodeplot(statistics=combat.frma.res$res.t, index=c2Sets[["PYEON_HPV_POSITIVE_TUMORS_UP"]])
barcodeplot(statistics=sva.frma.res$res.t, index=c2Sets[["PYEON_HPV_POSITIVE_TUMORS_UP"]])
barcodeplot(statistics=sva.combat.frma.res$res.t, index=c2Sets[["PYEON_HPV_POSITIVE_TUMORS_UP"]])
dev.off()

ProjectTemplate::cache("frma.res")
ProjectTemplate::cache("combat.frma.res")
ProjectTemplate::cache("sva.frma.res")
ProjectTemplate::cache("sva.combat.frma.res")
ProjectTemplate::cache("frma.tab")
ProjectTemplate::cache("combat.frma.tab")
ProjectTemplate::cache("sva.frma.tab")
ProjectTemplate::cache("sva.combat.frma.tab")
ProjectTemplate::cache("tabgenesets")
