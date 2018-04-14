# tar the Rlibs directories for R/qtl2 packages
# ...and place them in the qtl2cran/bin/macosx/ folder

setwd("~/Code/Rqtl2")
files <- list.files(pattern="\\.tar\\.gz$")

spl <- strsplit(files, "_")
pkg <- sapply(spl, "[", 1)
tgzfiles <- sub(".tar.gz", ".tgz", files)

setwd("~/Rlibs")
for(i in seq_along(pkg)) {
    cat(" -Creating", tgzfiles[i], "\n")
    system(paste("tar czf", tgzfiles[i], pkg[i]))
    system(paste("mv", tgzfiles[i], "~/Code/Rqtl2/miniCRAN/qtl2cran/bin/macosx/el-capitan/contrib/3.4/"))
}
