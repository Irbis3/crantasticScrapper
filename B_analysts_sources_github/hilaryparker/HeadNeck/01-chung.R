# Munging Chung data #

##### Annotation #####
info.chung <- reducedAnno

## Clean up Disease.state variable ##
info.chung$Disease.state[info.chung$Disease.state=="AWD "] <- "AWD"
info.chung$Disease.state[info.chung$Disease.state=="DOD "] <- "DOD"
info.chung$Disease.state[info.chung$Disease.state=="DOC "] <- "DOC"


##### Preprocessing expression data ######
frma.chung <- exprs(frma(dat.chung))


##### matching info.chung to data #####
info.chung <- info.chung[match(colnames(frma.chung),info.chung$Affy.Microarray),]


##### Removing unwanted samples from the analysis #####
## Remove blood samples ##
frma.chung <- frma.chung[,!info.chung$Tumor.Source=="Blood"]
info.chung <- info.chung[!info.chung$Tumor.Source=="Blood",]

## Remove Recurrent Primary Tumors ##
frma.chung <- frma.chung[,!info.chung$Tumor.Source=="Recurrent Primary"]
info.chung <- info.chung[!info.chung$Tumor.Source=="Recurrent Primary",]

## Remove OCT procurement ##
frma.chung <- frma.chung[,!info.chung$Procurement=="OCT"]
info.chung <- info.chung[!info.chung$Procurement=="OCT",]

## Remove Normal Samples ##
frma.chung <- frma.chung[,-c(grep("Normal",info.chung$Tumor.Source))]
info.chung <- info.chung[-c(grep("Normal",info.chung$Tumor.Source)),]

## save set with unlabeled HPV samples to predict later ##
frma.chung.naHPV <- frma.chung[,is.na(info.chung$HPV.Stat)]
info.chung.naHPV <- info.chung[is.na(info.chung$HPV.Stat),]
ProjectTemplate::cache('info.chung.naHPV')
ProjectTemplate::cache('frma.chung.naHPV')

## Remove samples with no HPV information ##
frma.chung <- frma.chung[,!is.na(info.chung$HPV.Stat)]
info.chung <- info.chung[!is.na(info.chung$HPV.Stat),]

dim(frma.chung)
# [1] 54675    86
dim(info.chung)
# [1] 86 12
ProjectTemplate::cache('info.chung')
ProjectTemplate::cache('frma.chung')


##### ComBat Batch correction (HPV outcome, Procurement method as batch) #####
## batch example motivated by plots of microarrays ##

# ComBat Only #
mod <- matrix(nrow=length(as.factor(info.chung$HPV.Stat)),ncol=1,as.factor(info.chung$HPV.Stat))
combat.frma.chung <- sva::ComBat(frma.chung,info.chung$Procurement,mod)
ProjectTemplate::cache('combat.frma.chung')

# SVA Only
mod <- model.matrix(~as.factor(info.chung$HPV.Stat))
sv <- sva(frma.chung,mod)
modSv <- cbind(mod, sv$sv)
nmod <- dim(mod)[2]
gammahat <- (frma.chung %*% modSv %*% solve(t(modSv) %*% modSv))[, (nmod +
        1):(nmod + sv$n.sv)]
sva.frma.chung <- frma.chung - gammahat %*% t(sv$sv)
ProjectTemplate::cache('sva.frma.chung')

# SVA correction after ComBat(HPV outcome) #
mod <- model.matrix(~as.factor(info.chung$HPV.Stat))
sv <- sva(combat.frma.chung,mod)
modSv <- cbind(mod, sv$sv)
nmod <- dim(mod)[2]
gammahat <- (combat.frma.chung %*% modSv %*% solve(t(modSv) %*% modSv))[, (nmod +
        1):(nmod + sv$n.sv)]
sva.combat.frma.chung <- combat.frma.chung - gammahat %*% t(sv$sv)
ProjectTemplate::cache('sva.combat.frma.chung')



## obtain gene sets ##
c2Sets <- GSA.read.gmt(filename=('data/c2.all.v3.1.entrez.gmt'))
names(c2Sets$genesets) <- c2Sets$geneset.names
c2Sets <- c2Sets$genesets
c2Sets <- lapply(c2Sets,toupper)
ProjectTemplate::cache('c2Sets')