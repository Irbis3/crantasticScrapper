tab <- read.delim("E-GEUV-1.sdrf.txt")
colnames(tab) <- 1:ncol(tab)
tab <- tab[,c(2,7,20,21,28,29)]
colnames(tab) <- c("sample","pop","perf","assay","run","url")
tab$date <- sub(".*M_(.*)_.","\\1",tab$assay)
tab <- tab[order(tab$perf, tab$pop),]
write.table(tab, file="samples.txt", quote=FALSE, sep="\t", row.names=FALSE)
out <- capture.output( table(tab$date, tab$perf, tab$pop)/2 )
write(out, file="table.txt")
