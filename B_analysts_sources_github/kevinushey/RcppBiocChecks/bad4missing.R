setwd("~/RcppBiocChecks/")
lib <- normalizePath("libs")
Sys.setenv(R_LIBS_USER=lib)

## A function to grab a package, plus dependencies.
get_and_install <- function(x, withDeps=FALSE, ...) {

  tryCatch(
    install.packages(x, lib=lib, destdir=normalizePath("tarballs"),
      dependencies=c("Depends", "Imports", "LinkingTo", "Suggests")
    ), error=function(e) {
      biocLite(x,
        lib=lib, destdir=normalizePath("tarballs"),
        dependencies=c("Depends", "Imports", "LinkingTo", "Suggests")
      )
    }
  )
}

get <- function(x) {
  download.packages(x, "tarballs", repos=c(getOption("repos"), biocinstallRepos()))
}

bad4missing <- c("CARBayes",            # ‘deldir’ ‘maptools’ ‘shapefiles’ ‘spdep’
                 "CDM",                 # ‘psych’ ‘polycor’
                 "classify",            # ‘R2WinBUGS’ ‘R2jags’
                 "diversitree",         # ‘deSolve’ ‘subplex’
                 "geiger",              # ‘msm’ ‘subplex’ ‘deSolve’ ‘coda’ ‘ncbit’
                 "GeneticTools",        # ‘gMWT’ ‘snpStats’
                 "GOsummaries",         # ‘gProfileR’ ‘limma’
                 "hypervolume",         # raster, maps
                 "kmc",                 # ‘rootSolve’ ‘emplik’
                 "orQA",                # ‘genefilter’ [from BioC]
                 "sirt",                # ‘gregmisc’ ‘sfsmisc’ ‘TAM’ ‘CDM’ ‘ic.infer’ ‘psych’ ‘pbivnorm’
                 "snplist",             #" 'biomaRt' [from BioC]
                 "surveillance",        # ‘polyCub’ ‘spatstat’
                 "TAM",                 # ‘tensor’ ‘sfsmisc’ ‘GPArotation’ ‘psych’
                 "tbart")               # ‘GISTools’ (and maptools, geos)



installed_pkgs <- rownames(installed.packages())
to_install <- bad4missing[ !(bad4missing %in% installed_pkgs) ]

if (length(to_install)) {
  stop("")
}

check <- function(pkg) {

  tarballs <- list.files("tarballs", pattern=pkg, full.names=TRUE)
  if (!length(tarballs)) {
    cat("No tarball for pkg '", pkg, "' available!")
    return(NA)
  }
  tarball <- tarballs[1]
  cat("Using tarball '", tarball, "' for installation.\n", sep="")

  cat("Starting check for package '", pkg, "'.\n", sep="")
  rc <- system(paste("R CMD check --no-manual --no-vignettes ", tarball, " > ", "logs/", pkg, ".log", sep=""))
  time <- strftime( Sys.time() )
  if (rc == 0) {
    cat("Package '", pkg, "' checked successfully at ", time, ".\n\n", sep="")
  } else {
    cat("Package '", pkg, "' failed R CMD check at ", time, ".\n\n", sep="")
  }
  return(rc)
}

codes <- lapply(bad4missing, check)

res <- data.frame(
  pkg=bad4missing,
  code=unlist(codes),
  stringsAsFactors=FALSE
)

logs <- list.files("logs")

print(res)
write.table(res, file=paste("results/result-", strftime(Sys.time(), "%Y%m%d-%H%M%S"), ".txt", sep=""), sep=",")
save(res, file=paste("result-", strftime(Sys.time(), "%Y%m%d-%H%M%S"), ".RData", sep=""))
cat("Ended at ", format(Sys.time()), "\n")

## Collate all the most relevant results into a nice repository
lapply(res$pkg, function(pkg) {
  dir.create( file.path("results", pkg), showWarnings=FALSE, recursive=TRUE )

  ## copy the log file
  pkg_logs <- sort(list.files("logs", pattern=pkg, full.names=TRUE))
  if (length(pkg_logs)) {
    pkg_log <- pkg_logs[1]
    filename <- gsub(".*/(.*)\\.log$", "\\1", pkg_log)
    file.copy(pkg_log, file.path("results", pkg, paste0(filename, ".log")), overwrite=TRUE)
  }

  ## copy relevant output from R CMD check, if available
  Rcheck <- paste0(pkg, ".Rcheck")
  dest <- file.path("results", pkg, Rcheck)
  if (file.exists(Rcheck)) {
    ## copy the check and install logs, if possible
    install.log <- file.path(Rcheck, "00install.out")
    if (file.exists(install.log)) {
      file.copy(install.log, file.path("results", pkg, paste0(pkg, "-install.log")), overwrite=TRUE)
    }

    check.log <- file.path(Rcheck, "00check.log")
    if (file.exists(check.log)) {
      file.copy(check.log, file.path("results", pkg, paste0(pkg, "-check.log")), overwrite=TRUE)
    }
  }

})
