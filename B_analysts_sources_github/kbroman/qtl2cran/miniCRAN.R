# download R/qtl2 dependencies and place in qtl2cran/

if(!require("miniCRAN")) install.packages("miniCRAN")

library(miniCRAN)

repos <- "https://cran.rstudio.com"

pkgs <- c("Rcpp", "RcppEigen", "yaml", "jsonlite", "data.table",
          "knitr", "rmarkdown", "qtl", "qtlcharts", "optparse", "RSQLite")
pkgList <- pkgDep(pkgs, repos=repos, type="source", suggests=FALSE)

makeRepo(pkgList, path="qtl2cran", repos=repos, type=c("source", "win.binary", "mac.binary.el-capitan"))
