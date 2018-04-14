# Install remote packages using devtools
# Remote packages naming specification:
# https://cran.r-project.org/web/packages/devtools/vignettes/dependencies.html

stopifnot(require('devtools'))

install_remotes = function(pkg) {

  pkg_prefix = sapply(strsplit(pkg, '::'), '[', 1)

  pkg_github    = union(which(!grepl('::', pkg)), which(pkg_prefix == 'github'))
  pkg_git       = which(pkg_prefix == 'git')
  pkg_bitbucket = which(pkg_prefix == 'bitbucket')
  pkg_bioc      = which(pkg_prefix == 'bioc')
  pkg_svn       = which(pkg_prefix == 'svn')
  pkg_url       = which(pkg_prefix == 'url')
  pkg_local     = which(pkg_prefix == 'local')

  if (length(pkg_github) != 0L)
    devtools::install_github(
      gsub('^github::', '', pkg[pkg_github]))

  if (length(pkg_git) != 0L)
    devtools::install_git(
      sapply(strsplit(pkg[pkg_git], '::'), '[', 2))

  if (length(pkg_bitbucket) != 0L)
    devtools::install_bitbucket(
      sapply(strsplit(pkg[pkg_bitbucket], '::'), '[', 2))

  if (length(pkg_bioc) != 0L)
    devtools::install_bioc(
      sapply(strsplit(pkg[pkg_bioc], '::'), '[', 2))

  if (length(pkg_svn) != 0L)
    devtools::install_svn(
      sapply(strsplit(pkg[pkg_svn], '::'), '[', 2))

  if (length(pkg_url) != 0L)
    devtools::install_url(
      sapply(strsplit(pkg[pkg_url], '::'), '[', 2))

  if (length(pkg_local) != 0L)
    devtools::install_local(
      sapply(strsplit(pkg[pkg_local], '::'), '[', 2))

}
