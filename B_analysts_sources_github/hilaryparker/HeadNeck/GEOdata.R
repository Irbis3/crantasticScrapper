## obtaining data from GEO ##
## this process involves both R and unix commands ##
## putting into a separate folder from munge -- can be rerun, but requires manual ##
##   input from user. Is not automatic (as is everything else in munge folder ##

# unix commands to get started #
cd /home/bst/student/hiparker/HeadNeck/GEO_celfiles
R

# R commands
library(GEOquery)

getGEOSuppFiles(GEO="GSE3292", makeDirectory = TRUE)
getGEOSuppFiles(GEO="GSE6791", makeDirectory = TRUE)
getGEOSuppFiles(GEO="GSE9844", makeDirectory = TRUE)

q('no')
##

# unix commands #
cd GSE3292
tar -xvf GSE3292_RAW.tar
cd ..

cd GSE6791
tar -xvf GSE6791_RAW.tar
cd ..

cd GSE9844
tar -xvf GSE9844_RAW.tar
cd ..
cd ..

R

# R commands #
library(affy)

# GSE6791 #
setwd('~/HeadNeck/GEO_celfiles/GSE6791')
files.6791 <- list.celfiles(pattern = ".CEL")
dat.6791 <- ReadAffy(filenames=files.6791, verbose=TRUE)
setwd("~/HeadNeck/data")
save('dat.6791',file="dat.6791.RData")

# GSE3292 #
setwd('~/HeadNeck/GEO_celfiles/GSE3292')
files.3292 <- list.celfiles(pattern = ".CEL")
dat.3292 <- ReadAffy(filenames=files.3292, verbose=TRUE)
setwd("~/HeadNeck/data")
save('dat.3292',file="dat.3292.RData")

# GSE9844 #
setwd('~/HeadNeck/GEO_celfiles/GSE9844')
files.9844 <- list.celfiles(pattern = ".CEL")
dat.9844 <- ReadAffy(filenames=files.9844, verbose=TRUE)
setwd("~/HeadNeck/data")
save('dat.9844',file="dat.9844.RData")


