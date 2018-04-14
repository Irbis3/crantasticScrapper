setwd("/home/bst/student/hiparker/HeadNeck")
library("ProjectTemplate")
load.project()

## p16 analysis ##

# knew ahead of time symbol was CDKN2A (in literature) #
# wanted to get gene name to be sure #

x <- hgu133plus2SYMBOL
y <- hgu133plus2GENENAME
xx <- as.list(x)
yy <- as.list(y)

# match to known gene symbol #
temp <- grep("CDKN2A",xx)
yy[temp[1]]
# $`1554348_s_at`
# [1] "CDKN2A interacting protein N-terminal like"
yy[temp[2]]
# $`207039_at`
# [1] "cyclin-dependent kinase inhibitor 2A"
yy[temp[3]]
# $`209644_x_at`
# [1] "cyclin-dependent kinase inhibitor 2A"
yy[temp[4]]
# $`211156_at`
# [1] "cyclin-dependent kinase inhibitor 2A"
# probably measuring the end of the gene
#alternative splicing
#bad hybridization
#falls off towards end of the gene
#RNA sequencing better than arrays
yy[temp[5]]
# $`218929_at`
# [1] "CDKN2A interacting protein"
yy[temp[6]]
# $`235006_at`
# [1] "CDKN2A interacting protein N-terminal like"

# 2, 3, 4 are the ones we want
temp[2]
# [1] 16486
temp[3]
# [1] 19054
temp[4]
# [1] 20520


# checking the change in differential expression for before/after

# set up table for paper
ttable<-matrix(nrow=3,ncol=2)
colnames(ttable)<-c("Before Batch Correction","After Batch Correction")
rownames(ttable)<-names(yy[temp[2:4]])

#temp[2]
ttable[1,1]<-round(t.test(frma.chung[temp[2],out=="Pos"],frma.chung[temp[2],out=="Neg"])$statistic,2)
#t.test(combat.frma.chung[temp[2],out=="Pos"],combat.frma.chung[temp[2],out=="Neg"]) 
ttable[1,2]<-round(t.test(sva.combat.frma.chung[temp[2],out=="Pos"],sva.combat.frma.chung[temp[2],out=="Neg"])$statistic,2)

#temp[3]
ttable[2,1]<-round(t.test(frma.chung[temp[3],out=="Pos"],frma.chung[temp[3],out=="Neg"])$statistic,2)
#t.test(combat.frma.chung[temp[3],out=="Pos"],combat.frma.chung[temp[3],out=="Neg"])
ttable[2,2]<-round(t.test(sva.combat.frma.chung[temp[3],out=="Pos"],sva.combat.frma.chung[temp[3],out=="Neg"])$statistic,2)

#temp[4]
ttable[3,1]<-round(t.test(frma.chung[temp[4],out=="Pos"],frma.chung[temp[4],out=="Neg"])$statistic,2)
#t.test(combat.frma.chung[temp[4],out=="Pos"],combat.frma.chung[temp[4],out=="Neg"])
ttable[3,2]<-round(t.test(sva.combat.frma.chung[temp[4],out=="Pos"],sva.combat.frma.chung[temp[4],out=="Neg"])$statistic,2)
# goes from not significant to significant!

ProjectTemplate::cache("ttable")

cols <- brewer.pal(3, "Dark2")
cols <- cols[2:3]
cols<-c(cols,cols)

pretty_boxplot(y=list(frma.chung[temp[2],out=="Pos"],frma.chung[temp[2],out=="Neg"],sva.combat.frma.chung[temp[2],out=="Pos"],sva.combat.frma.chung[temp[2],out=="Neg"]),cols=cols,labs=c("HPV Positive","HPV Negative","HPV Positive","HPV Negative"),main="Probe 207039_at")

pretty_boxplot(y=list(),cols=cols,labs=c("HPV Positive","HPV Negative"),main="Probe 207039_at")

pretty_boxplot(y=list(frma.chung[temp[3],out=="Pos"],frma.chung[temp[3],out=="Neg"]),cols=cols,labs=c("No Correction","SVA + ComBat Correction"),main="Probe 209644_x_at")

pretty_boxplot(y=list(frma.chung[temp[4],out=="Pos"],frma.chung[temp[4],out=="Neg"]),cols=cols,labs=c("No Correction","SVA + ComBat Correction"),main="Probe 211156_at")






########### end of curated code


setwd("C:/Users/Hilary/GitHub/HeadNeck/graphs")

# boxplots for database
png(file="p16nocorrection.png")
boxplot(list(HPVPos=frma.chung[temp[2],out=="Pos"],HPVNeg=frma.chung[temp[2],out=="Neg"]), main="p16 Expression, No Correction")
dev.off()

png(file="p16svacombat.png")
boxplot(list(HPVPos=sva.combat.frma.chung[temp[2],out=="Pos"],HPVNeg=sva.combat.frma.chung[temp[2],out=="Neg"]), main="p16 Expression, Batch Correction")
dev.off()

# plots for new samples

# no correction predictions #
y1<-frma.chung.naHPV[temp[2], pred.none=="Pos"]
y2<-frma.chung.naHPV[temp[2], pred.none=="Neg"]
x1<-rep(0,length(y1))
x2<-rep(1,length(y2))
y<-c(y1,y2)
x<-c(x1,x2)
plot(x,y,pch=16)

png(file="p16nocorrection_newsamps.png")
boxplot(list(HPVPos=frma.chung.naHPV[temp[2], pred.none=="Pos"],HPVNeg=frma.chung.naHPV[temp[2], pred.none=="Neg"]),main="p16 Expression in new samples, No Correction")
dev.off()

# sva correction predictions #
y1<-frma.chung.naHPV[temp[2], pred.sva=="Pos"]
y2<-frma.chung.naHPV[temp[2], pred.sva=="Neg"]
x1<-rep(0,length(y1))
x2<-rep(1,length(y2))
y<-c(y1,y2)
x<-c(x1,x2)
plot(x,y,pch=16)

# sva+fsva correction predictions #
y1<-frma.chung.naHPV[temp[2], pred.sva.fsva=="Pos"]
y2<-frma.chung.naHPV[temp[2], pred.sva.fsva=="Neg"]
x1<-rep(0,length(y1))
x2<-rep(1,length(y2))
y<-c(y1,y2)
x<-c(x1,x2)
plot(x,y,pch=16)

# combat correction predictions
y1<-frma.chung.naHPV[temp[2], pred.combat=="Pos"]
y2<-frma.chung.naHPV[temp[2], pred.combat=="Neg"]
x1<-rep(0,length(y1))
x2<-rep(1,length(y2))
y<-c(y1,y2)
x<-c(x1,x2)
plot(x,y,pch=16)

# combat + fsva predictions
y1<-frma.chung.naHPV[temp[2], pred.combat.fsva=="Pos"]
y2<-frma.chung.naHPV[temp[2], pred.combat.fsva=="Neg"]
x1<-rep(0,length(y1))
x2<-rep(1,length(y2))
y<-c(y1,y2)
x<-c(x1,x2)
plot(x,y,pch=16)


# combat + sva predictions
y1<-frma.chung.naHPV[temp[2], pred.sva.combat=="Pos"]
y2<-frma.chung.naHPV[temp[2], pred.sva.combat=="Neg"]
x1<-rep(0,length(y1))
x2<-rep(1,length(y2))
y<-c(y1,y2)
x<-c(x1,x2)
plot(x,y,pch=16)


# combat + sva + fsva predictions
y1<-frma.chung.naHPV[temp[2], pred.sva.combat.fsva=="Pos"]
y2<-frma.chung.naHPV[temp[2], pred.sva.combat.fsva=="Neg"]
x1<-rep(0,length(y1))
x2<-rep(1,length(y2))
y<-c(y1,y2)
x<-c(x1,x2)

png(file="p16corrected_newsamps.png")
plot(x,y,pch=16,main="p16 Expression in new samples, Batch Correction",xlim=c(-1,2))
dev.off()

png(file="p16svacombatfsva_newsampes.png")
boxplot(list(frma.chung.naHPV[temp[2], pred.sva.fsva=="Pos"],frma.chung.naHPV[temp[2], pred.sva.fsva=="Neg"]))
boxplot(list(frma.chung.naHPV[temp[2], pred.sva.combat.fsva=="Pos"],frma.chung.naHPV[temp[2], pred.sva.combat.fsva=="Neg"]))
boxplot(list(frma.chung.naHPV[temp[2], pred.sva.combat.fsva=="Pos"],frma.chung.naHPV[temp[2], pred.sva.combat.fsva=="Neg"]))
boxplot(list(frmadat.combat[temp[2],out=="Pos"],frmadat.combat[temp[2],out=="Neg"]))
boxplot(list(frmadat.combat.sva[temp[2],out=="Pos"],frmadat.combat.sva[temp[2],out=="Neg"]))





## boxplots of standard deviations - see what's going down in variance
## try to do a boxplot function if possible
## want to see HPV Pos going down in standard deviation
# sva only

# scientific first so that Christine can get 
# comfortable story before going to lab meeting

# correlate SV's with batch variables, see if they match up.
# both when done with ComBat and without--maybe it doesn't that's OK
# might correlate to more than one.

# don't spend too much time perfecting the boxplots

# does combat+sva make frozen + ffpe look more similar? That would be a really strong figure
# just worry about it in picture form for the time being

# combat just with amplification kit is fine.
# also do SVA alone and see how it performs.

#gene set statistics between positive and negative based on 



t.test(frmadat[temp[3],out=="Pos"],frmadat[temp[3],out=="Neg"])
# t = 6.0648
# mean of x mean of y
#  8.355814  6.611019

t.test(frmadat.combat[temp[3],out=="Pos"],frmadat.combat[temp[3],out=="Neg"])
# t = 7.0975
# mean of x mean of y
#  8.430110  6.577763

t.test(frmadat.combat.sva[temp[3],out=="Pos"],frmadat.combat.sva[temp[3],out=="Neg"])
# t = 7.8411
# mean of x mean of y
#  8.262802  6.658532


t.test(frmadat[temp[4],out=="Pos"],frmadat[temp[4],out=="Neg"])
# t = 1.4146 not significant
# mean of x mean of y
#  5.504952  4.976034

t.test(frmadat.combat[temp[4],out=="Pos"],frmadat.combat[temp[4],out=="Neg"])
# t = 1.2258 not significant
# mean of x mean of y
#  5.341518  5.105388

t.test(frmadat.combat.sva[temp[4],out=="Pos"],frmadat.combat.sva[temp[4],out=="Neg"])
# t = -0.3264 really not significant
# mean of x mean of y
#  5.158745  5.193623





datlist<-list(nulldat,combatdat)
dat<-mergeData(datlist,idCol=1,byCol=2)

# decreasing absolute

temp1<-computeCat(dat,idCol=1,method="equalRank",decreasing=TRUE)
temp2<-computeCat(data = dat, idCol = 1,
ref="dataSetA.t", method="equalRank", decreasing=TRUE)

temp<-computeCat(mat,method="equalRank",decreasing=TRUE)
temp2<-calcHypPI(frmadat[1:10000,])

pdf(file="temp.pdf")
plotCat(catData=temp,preComputedPI=temp2)
dev.off()

## where are normal samples ##


negdat<-frmadat[,info$HPV.Stat=="Neg"]

out<-as.numeric(info$Disease.state=="DOD")[info$HPV.Stat=="Neg"] #outcome is those that died of disease (DOD)

negdat<-negdat[,!is.na(out)]
out<-out[!is.na(out)]

