tab <- read.delim("samples.txt")
write(as.character(tab$url[tab$pop == "TSI"]), file="tsi.files")
