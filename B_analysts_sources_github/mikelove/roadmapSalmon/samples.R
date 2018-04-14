library(readr)
samps <- read_tsv("samples.tsv")
table(samps$SRA_Sample_s)
samps.sub <- samps[!duplicated(samps$SRA_Sample_s),]
write_tsv(samps.sub, "samples_sub.tsv")
write(samps.sub$Run_s, "srrfiles")
