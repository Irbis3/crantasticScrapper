### installed binary of ...
### R version 3.3.1 (2016-06-21) -- "Bug in Your Hair"
### Copyright (C) 2016 The R Foundation for Statistical Computing
### Platform: x86_64-apple-darwin13.4.0 (64-bit)

## at first,
curr_pkgs <- data.frame(installed.packages())
nrow(curr_pkgs)
## 600!

## check if anything *other* than base and recommended packages are
## in the standard library
std_lib_pkgs <- data.frame(installed.packages(lib.loc = .Library))
nrow(std_lib_pkgs) ## 29
table(std_lib_pkgs$Priority)
## no, GOOD

## update packages in the standard library
update.packages(lib.loc = .Library)
## this resulted in R asking whether it should get newer versions of
## mgcv and survival
## I said yes

oth_pkgs <- subset(curr_pkgs, (!Priority %in% c("base", "recommended")))
summary(oth_pkgs$LibPath)
# /Library/Frameworks/R.framework/Versions/3.3/Resources/library
# 0
# /Users/jenny/resources/R/library
# 571

write.table(subset(oth_pkgs, select = c(Package, Version)),
            file = "/Users/jenny/resources/system-setup/2016-07-30_old-CRAN-pkgs.tsv",
            quote = FALSE, sep = "\t", row.names = FALSE)

update.packages(lib.loc = .libPaths()[1])

curr_pkgs <- data.frame(installed.packages())
oth_pkgs <- subset(curr_pkgs, (!Priority %in% c("base", "recommended")))
nrow(oth_pkgs)

write.table(subset(oth_pkgs, select = c(Package, Version)),
            file = "/Users/jenny/resources/system-setup/2016-07-30_CRAN-pkgs-after-upgrade.tsv",
            quote = FALSE, sep = "\t", row.names = FALSE)

