if (Sys.getenv("R_INSTALL_PKG") == "") local({
  .path <- file.path("~", "R-lib", sprintf("%s.%s", getRversion()$major, getRversion()$minor))
  .path <- normalizePath(.path)
  if (!dir.exists(.path)) {
    dir.create(.path, recursive = TRUE)
  }
  .libPaths(new = .path)
  options(repos = c(CRAN = "http://cran.csie.ntu.edu.tw"))
})
