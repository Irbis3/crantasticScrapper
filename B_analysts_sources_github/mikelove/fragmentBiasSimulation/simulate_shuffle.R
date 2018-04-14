# this process uses ~2 Gb on my cluster and ~0:45 hr

library(Rsamtools)
library(Biostrings)
simTypes <- c("confounded","balanced")
for (st in simTypes) {
  for (i in 1:30) {
    print(i)
    samp <- paste0("sample_",ifelse(i < 10,"0",""),i)
    # simulation reads
    simfile1 <- paste0(samp,"_1.fasta")
    simfile2 <- paste0(samp,"_2.fasta")
    # read them in
    system.time({ sim1 <- readDNAStringSet(file.path("fasta",st,simfile1)) })
    system.time({ sim2 <- readDNAStringSet(file.path("fasta",st,simfile2)) })
    stopifnot(length(sim1) == length(sim2))
    # shuffle both reads with same index
    idx <- sample(length(sim1), length(sim1), replace=FALSE)
    out1 <- paste0("shuffle_",i,"_1.fasta")
    out2 <- paste0("shuffle_",i,"_2.fasta")
    system.time({ writeXStringSet(sim1[idx], file=file.path("fasta",st,out1)) })
    system.time({ writeXStringSet(sim2[idx], file=file.path("fasta",st,out2)) })
  }
}
