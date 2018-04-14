### installed binary of ...
### R version 3.4.1 (2017-06-30) -- "Single Candle"
### Copyright (C) 2017 The R Foundation for Statistical Computing
### Platform: x86_64-apple-darwin15.6.0 (64-bit)

## at first,
curr_pkgs <- data.frame(installed.packages())
nrow(curr_pkgs)
## 29

## check if anything *other* than base and recommended packages are
## in the standard library
std_lib_pkgs <- data.frame(installed.packages(lib.loc = .Library))
nrow(std_lib_pkgs) ## 29
table(std_lib_pkgs$Priority)
## no, GOOD

## update packages in the standard library
update.packages(lib.loc = .Library)
## this resulted in R asking whether it should get newer versions of
## boot, Matrix, mgcv
## I said yes

oth_pkgs <- subset(curr_pkgs, (!Priority %in% c("base", "recommended")))
summary(oth_pkgs$LibPath)
# /Library/Frameworks/R.framework/Versions/3.4/Resources/library
# 0

## put old library on the path so I can see what was there
.libPaths("~/resources/2017-09_old-resources/R/library/")
curr_pkgs <- data.frame(installed.packages())
nrow(curr_pkgs)
## 851
oth_pkgs <- subset(curr_pkgs, (!Priority %in% c("base", "recommended")))
summary(oth_pkgs$LibPath)
## 822 packages in the old library
write.table(subset(oth_pkgs, select = c(Package, Version)),
            file = "/Users/jenny/resources/system-setup/2017-09-04_old-CRAN-pkgs.tsv",
            quote = FALSE, sep = "\t", row.names = FALSE)

## restarted R here
prev_pkgs <- read.delim("/Users/jenny/resources/system-setup/2017-09-04_old-CRAN-pkgs.tsv",
                        stringsAsFactors = FALSE)

## re-install anything for which there is a CRAN version
install.packages(prev_pkgs$Package)

## Warning in install.packages :
## packages BLAH BLAH BLAH (BioC and GitHub-only stuff)
## are not available (for R version 3.4.1)

## authorized installation from source of bayesplot, Epi, OpenMx, psych, qdap, shinystan, storr
## then also these specific ones with C/C++ code: cairoDevice, playwith, RGtk2, wand

## I now regret my Makevars, leftover from readxl checking

## here are (some of) the packages with problems: OpenMx, rJava, RGtk2, wand, playwith

## how'd we do?
curr_pkgs <- data.frame(installed.packages())
nrow(curr_pkgs)
## 777

not_yet <- setdiff(prev_pkgs$Package, curr_pkgs$Package)
length(not_yet)
## 78

## on CRAN, but I did not successfully installed
available <- available.packages()
intersect(available[ , "Package"], not_yet)
# [1] "cairoDevice"   "gWidgetsRGtk2" "OpenMx"        "playwith"      "qdap"
# [6] "RGtk2"         "wand"

## I will deal with these if / as needed
## I recognize most from readxl revdep check adventure

update.packages(lib.loc = .libPaths()[1])
## nothing needs update ... yay

## so, what are the GitHub only packages?
setdiff(not_yet, available[ , "Package"])

## mostly I will wait til I recognize a need for these but I will install a few now
install_github("jimhester/lookup")
install_github("hadley/pkgdown")
## that caused install from github of pkgload, pkgbuild, callr, rlang, rstudioapi, usethis
install_github("hadley/tidytemplate")

## record where we ended up at the end of this effort
curr_pkgs <- data.frame(installed.packages())
nrow(curr_pkgs)
## 783
oth_pkgs <- subset(curr_pkgs, (!Priority %in% c("base", "recommended")))
summary(oth_pkgs$LibPath)
## 754

write.table(subset(oth_pkgs, select = c(Package, Version)),
            file = "/Users/jenny/resources/system-setup/2017-09-04_pkgs-after-upgrade.tsv",
            quote = FALSE, sep = "\t", row.names = FALSE)
