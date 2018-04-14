
globalwd<-"/home/bst/student/hiparker/PracticalBatch"
## working directory of your files ##


################################################################################
################################################################################
##                              Section 3                                     ##
################################################################################
################################################################################

globalwd<-"/home/bst/student/hiparker/PracticalBatch"
# Note that figure post-processing performed in PowerPoint #
setwd(globalwd)
pdf(file="batchhist.pdf",width=14)
par(mfrow=c(1,2))
load("Section3_GSE2034.RData")
hist(dts,breaks=40,xlab="Experimental Day",main=" ",
			xlim=c(0,350), col="gray", cex.lab=1.3)
load("Section3_GSE2603.RData")
hist(dts,breaks=50,xlab="Experimental Day",main=" ",
			xlim=c(0,450), col="gray", cex.lab=1.3)
dev.off()


################################################################################
################################################################################
##                              Section 4                                     ##
################################################################################
################################################################################


globalwd<-"/home/bst/student/hiparker/PracticalBatch"

### Figures for Section 4 ###
setwd(globalwd)
pdf(file="GSE2034_cvplots.pdf",width=14)
load("Section4_GSE2034.RData")
source("cvplot.R")
cvplot(nrep=100,nbatch=2)
dev.off()

pdf(file="GSE2603_cvplots.pdf",width=14)
load("Section4_GSE2603.RData")
source("cvplot.R")
cvplot(nrep=100,nbatch=2)
dev.off()

### Tables for Section 4 ###

### NOTE:  Post-processing needed to make median values bold, change 1's to 1.000's ##
source("cvtable.R")

load("Section4_GSE2034.RData")
tab1<-cvtable(nrep=100,nbatch=2)
load("Section4_GSE2603.RData")
tab2<-cvtable(nrep=100,nbatch=2)
library(xtable)
xtable(rbind(tab1,tab2))


################################################################################
################################################################################
##                              Section 5                                     ##
################################################################################
################################################################################

globalwd<-"/home/bst/student/hiparker/PracticalBatch"
setwd(globalwd)
source("confplots.R")

pdf(file="confplots.pdf",height=14,width=14)
par(mfrow=c(2,2))
confplots(filename="Section5_GSE2034_fRMA_PAM.RData",PAM=TRUE,mainttl="fRMA-PAM")
confplots(filename="Section5_GSE2034_RMA_PAM.RData",PAM=TRUE,mainttl="RMA-PAM")
confplots(filename="Section5_GSE2034_fRMA_TSP.RData",PAM=FALSE,mainttl="fRMA-TSP")
confplots(filename="Section5_GSE2034_RMA_TSP.RData",PAM=FALSE,mainttl="RMA-TSP")
dev.off()


pdf(file="confplots_fRMA.pdf",height=7,width=14)
par(mfrow=c(1,2))
confplots(filename="Section5_GSE2034_fRMA_PAM.RData",PAM=TRUE,mainttl="fRMA-PAM")
confplots(filename="Section5_GSE2034_fRMA_TSP.RData",PAM=FALSE,mainttl="fRMA-TSP")
dev.off()

pdf(file="confplots_RMA.pdf",height=7,width=14)
par(mfrow=c(1,2))
confplots(filename="Section5_GSE2034_RMA_PAM.RData",PAM=TRUE,mainttl="RMA-PAM")
confplots(filename="Section5_GSE2034_RMA_TSP.RData",PAM=FALSE,mainttl="RMA-TSP")
dev.off()




################################################################################
################################################################################
##                              Section 6                                     ##
################################################################################
################################################################################


globalwd<-"/home/bst/student/hiparker/PracticalBatch"
setwd(globalwd)

source("Section6_densities.R")
get_densities(filename="GSE2034")
get_densities(filename="GSE2603")

source("confplot_reduced.R")
confplot_reduced(filename="Section6_GSE2034")
confplot_reduced(filename="Section6_GSE2603")
 
 
source("Section6_bad_ps_compare.R")
compare()
 
 

#Maybe for appendix?#
source("confplots.R")
pdf(file="test.pdf")
confplots(filename="Section5_GSE2034_fRMA_PAM.RData",PAM=TRUE,mainttl="fRMA-PAM (0%)")
confplots(filename="Section6_GSE2034_fRMA_PAM_10.RData",PAM=TRUE,mainttl="fRMA-PAM (10%)")
confplots(filename="Section6_GSE2034_fRMA_PAM_20.RData",PAM=TRUE,mainttl="fRMA-PAM (20%)")
confplots(filename="Section6_GSE2034_fRMA_PAM_30.RData",PAM=TRUE,mainttl="fRMA-PAM (30%)")
confplots(filename="Section6_GSE2034_fRMA_PAM_40.RData",PAM=TRUE,mainttl="fRMA-PAM (40%)")
confplots(filename="Section6_GSE2034_fRMA_PAM_50.RData",PAM=TRUE,mainttl="fRMA-PAM (50%)")
confplots(filename="Section6_GSE2034_fRMA_PAM_60.RData",PAM=TRUE,mainttl="fRMA-PAM (60%)")
dev.off()


source("Section6_multi_figs.R")
multi_figs(globalwd=globalwd,bgadj=TRUE)
multi_figs(globalwd=globalwd,bgadj=FALSE)
multi_figs_examples(globalwd=globalwd,bgadj=FALSE)
multi_figs_examples(globalwd=globalwd,bgadj=TRUE)
# Need to do a little tweaking of this table... #
source("Section6_minnwang_plots.R")
minnwang_figs(globalwd=globalwd)



