# storing the results of one iteration of the predictor code in order to plot results

setwd("/home/bst/student/hiparker/HeadNeck")
library("ProjectTemplate")
load.project()

dat <- frma.chung
out <- as.factor(info.chung$HPV.Stat)

set.seed(12345)
tmp<-c(rep(2,86-2*43),rep(0,43),rep(1,43))
ind<-sample(tmp,86,replace=FALSE)
db.dat<-dat[,ind==0]
db.out<-out[ind==0]
newsamp.dat<-dat[,ind==1]
newsamp.out<-out[ind==1]

# run sva on the database (will be used later in fsva) #
mod<-model.matrix(~as.factor(db.out))
db.sva<-sva(db.dat,mod)

fsva.res <- fsva(dbdat=db.dat, mod=mod, sv=db.sva, newdat=newsamp.dat, method="exact")	


# no SVA, no fSVA #
null1<-cbind(db.dat,newsamp.dat)

# yes SVA, no fSVA #
null2<-cbind(fsva.res$db,newsamp.dat)

# yes SVA, yes fSVA #
test<-cbind(fsva.res$db,fsva.res$new)

setwd("/home/bst/student/hiparker/HeadNeck/doc")

png(file="null1.png")
manyboxplot(testcase_res$null1,dotcol=cols[1],linecol=cols[2:4],vlines=43.5,main="No Correction")
dev.off()

png(file="null2.png")
manyboxplot(testcase_res$null2,dotcol=cols[1],linecol=cols[2:4],vlines=43.5,main="Batch Correction on Training Set Only")
dev.off()

png(file="test.png")
manyboxplot(testcase_res$test,dotcol=cols[1],linecol=cols[2:4],vlines=43.5,main="Batch Correction on Training Set and Test Set")
dev.off()
