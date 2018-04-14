#'@useDynLib recurrentR
.onLoad <- function(libname, pkgname) {
	loadRcppModules()
}