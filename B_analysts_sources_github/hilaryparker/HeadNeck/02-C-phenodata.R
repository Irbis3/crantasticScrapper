

# Get phenotypic data (from Sameer)
setwd(workdir)
pheno.6791<-read.table(file="gse6791soft.txt",sep="\t",stringsAsFactors=F,header=TRUE)
pheno.3292<-read.table(file="gse3292soft.txt",sep="\t",stringsAsFactors=F,header=TRUE)
temp<-read.table(file="GSE9844_series_matrix.txt",sep="\t",stringsAsFactors=F,header=FALSE)
cn<-temp[,1]
temp<-temp[,-1]
pheno.9844<-t(temp)
colnames(pheno.9844)<-cn



# Combine into one dataset, creating a batch variable to indicate study #
len1 <- dim(frma.6791)[2]
len2 <- dim(frma.3292)[2]
len3 <- dim(frma.9844)[2]
len <- len1+len2+len3
Study <- rep(NA,len)
Study[1:len1]<-"GSE6791"
Study[(len1+1):(len1+len2)]<-"GSE3292"
Study[(len1+len2+1):(len1+len2+len3)]<-"GSE9844"

dat<-cbind(frma.6791,frma.3292,frma.9844)

# created outcomes HPV and Tumor type #

# all of GSE9844 is HPV negative #
HPV <- rep(NA,len)
HPV[1:len1] <- pheno.6791$HPV
HPV[(len1+1):(len1+len2)] <- pheno.3292$HPV
HPV[(len1+len2+1):(len1+len2+len3)] <- "Negative"
HPV<-gsub("-.*","Negative",HPV)
HPV<-gsub("\\+.*","Positive",HPV)

Type <- rep(NA,len)
Type[1:len1] <- pheno.6791$Case
Type[(len1+1):(len1+len2)] <- "HN cancer "
Type[(len1+len2+1):(len1+len2+len3)] <- pheno.9844[,8]

# tumor subsite #
Subsite <- rep(NA,len)
Subsite[1:len1] <- pheno.6791$Anatomical.sites 
Subsite[(len1+1):(len1+len2)] <- NA
Subsite[(len1+len2+1):(len1+len2+len3)] <- as.vector(pheno.9844[,8])

vars <- as.data.frame(cbind(Study,HPV,Type,Subsite))

combined_dat<-list(dat,vars)
ProjectTemplate::cache(combined_dat)

