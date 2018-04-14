## Section 6 ##

globalwd<-"/home/bst/student/hiparker/PracticalBatch"
## working directory of your files ##

sepdata_rma <- "Section5_GSE2034_rma_sepdata.RData"
sepdata_frma <- "Section5_GSE2034_frma_sepdata.RData"
bad_ps_func <- "Section6_bad_ps.R"

setwd(globalwd)
load(sepdata_rma)
load(sepdata_frma)
source(bad_ps_func)


bad_ps_fun(celA=celA,celB=celB,
			  frmaA=frmaA,frmaB=frmaB,
              direct=globalwd,
			  filename="Section6_GSE2034")
			  
