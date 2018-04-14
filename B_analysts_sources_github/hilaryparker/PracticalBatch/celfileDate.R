require(affyio)
celfileDate <- function(filename) {
	h <- affyio::read.celfile.header(filename, info="full")
	date <- grep("/", strsplit(h$DatHeader, " ")[[1]], value=TRUE)
	date <- strsplit(date, split="/")[[1]]
	CC <- ifelse(substr(date[3],1,1)=="9", "19", "20")
	as.Date(paste(paste(CC, date[3], sep=""), date[1], date[2], sep="-"))
}
