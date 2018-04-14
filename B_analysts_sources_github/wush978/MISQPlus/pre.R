library(tools, quietly=TRUE)
cache.source <- function(file_name) {
	md5 <- md5sum(file_name)
	image_name <- sub(pattern=".R",replacement=paste(".", md5, ".cache.Rdata", sep=""), file_name)
	if (!file.exists(image_name)) {
		source(file_name)
		save.image(image_name)
	} else {
		load(image_name, envir = globalenv())
	}
}
