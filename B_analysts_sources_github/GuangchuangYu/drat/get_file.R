get_file <- function(dir, pkg, md=TRUE, repo="https://guangchuangyu.github.io/drat") {
    f <- list.files(dir, full.names=TRUE)
    pkgfile <- max(f[grep(pkg, f)])
    if (!md) {
        return(basename(pkgfile))
    }
    repo <- paste0(sub("/$", "", repo), "/")
    paste0("[", basename(pkgfile), "](", repo, pkgfile, ")")
}
