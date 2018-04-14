# Data obtained directly from collaborator #
# condensed into expression set, put into data folder #

# make sure you get a node that is big enough #

# unix code
qrsh -l mem_free=10G,h_vmem=12G
R

# R commands #
library(affy)

setwd("~/HeadNeck/chung_celfiles")
files<-dir()
dat.chung<-read.affybatch(files,verbose=TRUE)
setwd("~/HeadNeck/data")
save('dat.chung',file="dat.chung.RData")