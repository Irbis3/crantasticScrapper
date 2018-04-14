# Install current/archived packages with/without specified versions from CRAN

stopifnot(require('RCurl'))
stopifnot(require('devtools'))

install_cran = function(pkg) {

  pkg_name = sapply(strsplit(pkg, '/'), '[', 1)
  pkg_url = rep(NA, length(pkg))

  # check if pkg have specified version
  pkg_versioned = grepl('/', pkg)
  pkg_version = sapply(strsplit(pkg[pkg_versioned], '/'), '[', 2)
  pkg_url[!pkg_versioned] = pkg[!pkg_versioned]

  if (any(is.na(pkg_url))) {

    # check if versioned packages are archived now
    pkg_desc_url = paste0('https://cran.r-project.org/web/packages/',
                          pkg_name[pkg_versioned],
                          '/DESCRIPTION')
    pkg_archived = !sapply(pkg_desc_url, url.exists)
    pkg_url[which(pkg_versioned)[which(pkg_archived)]] =
      paste0('https://cran.r-project.org/src/contrib/Archive/',
             pkg_name[which(pkg_versioned)[which(pkg_archived)]],
             '/',
             pkg_name[which(pkg_versioned)[which(pkg_archived)]],
             '_',
             pkg_version[which(pkg_archived)],
             '.tar.gz')

    if (any(is.na(pkg_url))) {

      # check if the specified versions of the present packages
      # are the same with the newest version on CRAN
      pkg_name_present = pkg_name[which(pkg_versioned)[!pkg_archived]]
      desc_url_present = paste0('https://cran.r-project.org/web/packages/',
                                pkg_name_present,
                                '/DESCRIPTION')
      pkg_desc = lapply(desc_url_present,
                        function(x) {conn = url(x); y = read.dcf(conn);
                        on.exit(close(conn)); return(y)})
      pkg_ver_present = sapply(pkg_desc, function(x) x[, 'Version'])
      ver_match = pkg_version[!pkg_archived] == pkg_ver_present

      # version matched packages (present version is specified version)
      pkg_url[(which(pkg_versioned)[which(!pkg_archived)])[which(ver_match)]] =
        pkg_name_present[which(ver_match)]

      # version unmatched packages (present version is not the specified version)
      pkg_url[(which(pkg_versioned)[which(!pkg_archived)])[which(!ver_match)]] =
        paste0('https://cran.r-project.org/src/contrib/Archive/',
               pkg_name_present[which(!ver_match)],
               '/',
               pkg_name_present[which(!ver_match)],
               '_',
               pkg_version[which(!pkg_archived)][which(!ver_match)],
               '.tar.gz')
    }

  }

  pkg_type = grepl('https://cran.r-project.org', pkg_url)
  pkg_from_url = pkg_url[pkg_type]
  pkg_from_name = pkg_url[!pkg_type]

  if (length(pkg_from_name) != 0L)
    install.packages(pkg_from_name, repos = 'https://cran.rstudio.com')

  if (length(pkg_from_url) != 0L)
    install_url(pkg_from_url)

}
