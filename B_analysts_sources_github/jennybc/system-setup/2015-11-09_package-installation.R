### installed binary of ...
### R version 3.2.2 (2015-08-14) -- "Fire Safety"
### Copyright (C) 2015 The R Foundation for Statistical Computing
### Platform: x86_64-apple-darwin13.4.0 (64-bit)

### does anything already need update?
update.packages(lib.loc = .Library)
### a few packages were updated

## at first,
curr_pkgs <- data.frame(installed.packages())
nrow(curr_pkgs) ## 29

## check if anything *other* than base and recommended packages are
## in the standard library ... I hate that
std_lib_pkgs <- data.frame(installed.packages(lib.loc = .Library))
all(unique(std_lib_pkgs$Priority) %in% c('base', 'recommended'))

## what packages were installed on stolen machine?
# prev_pkgs <- data.frame(installed.packages(
#   lib.loc = "/Volumes/LaCie/Users/jenny/resources/R/library/"))
# write.csv(prev_pkgs[c('Package', 'Version')],
#           "~/resources/R/code/2015-11-07_pkgs-on-stolen-mbp.csv",
#           row.names = FALSE, quote = FALSE)
prev_pkgs <- read.csv("~/resources/R/code/2015-11-07_pkgs-on-stolen-mbp.csv",
                      stringsAsFactors = FALSE)

## re-install anything for which there is a CRAN version
install.packages(prev_pkgs$Package)

## what did I just install?
cran_pkgs <- data.frame(installed.packages(lib.loc = .libPaths()[1]))[c('Package', 'Version')]
cran_pkgs$Package <- as.character(cran_pkgs$Package)
cran_pkgs$Version <- as.character(cran_pkgs$Version)

names(prev_pkgs) <- c('Package', 'prev_version')

prev_pkgs$fresh_version <- cran_pkgs$Version[match(prev_pkgs$Package, cran_pkgs$Package)]
vcv <- Vectorize(compareVersion, vectorize.args = c("a", "b"))
prev_pkgs$comparison <- vcv(prev_pkgs$prev_version, prev_pkgs$fresh_version)
prev_pkgs$ok <- prev_pkgs$comparison <= 0

table(prev_pkgs$ok)
# FALSE  TRUE 
#    13   315 

subset(prev_pkgs, subset = !ok)
#           Package prev_version fresh_version    ok comparison
# 8          argufy        1.0.0          <NA> FALSE          1
# 20  BiocInstaller       1.18.4          <NA> FALSE          1
# 47       devtools   1.9.1.9000         1.9.1 FALSE          1
# 57          dplyr   0.4.3.9000         0.4.3 FALSE          1
# 63          EDAWR          0.1          <NA> FALSE          1
# 98        ggplot2   1.0.1.9003         1.0.1 FALSE          1
# 100            gh        1.0.0          <NA> FALSE          1
# 105  googlesheets   0.1.0.9000         0.1.0 FALSE          1
# 207      practice        0.1.0          <NA> FALSE          1
# 236        reprex   0.0.0.9001          <NA> FALSE          1
# 260     rsconnect      0.4.1.4          <NA> FALSE          1
# 303         tidyr   0.3.1.9000         0.3.1 FALSE          1
# 326       yuyuyuy   0.0.0.9000          <NA> FALSE          1

source("https://bioconductor.org/biocLite.R")
biocLite("BiocInstaller")
prev_pkgs$fresh_version[prev_pkgs$Package == "BiocInstaller"] <- 
  as.character(packageVersion("BiocInstaller"))

prev_pkgs <- subset(prev_pkgs, !Package %in% c('rsconnect', 'yuyuyuy'))

prev_pkgs$comparison <- vcv(prev_pkgs$prev_version, prev_pkgs$fresh_version)
prev_pkgs$ok <- prev_pkgs$comparison <= 0

table(prev_pkgs$ok)
# FALSE  TRUE 
#    10   316 

subset(prev_pkgs, subset = !ok)
#          Package prev_version fresh_version    ok comparison
# 8         argufy        1.0.0            NA FALSE          1
# 47      devtools   1.9.1.9000         1.9.1 FALSE          1
# 57         dplyr   0.4.3.9000         0.4.3 FALSE          1
# 63         EDAWR          0.1            NA FALSE          1
# 98       ggplot2   1.0.1.9003         1.0.1 FALSE          1
# 100           gh        1.0.0            NA FALSE          1
# 105 googlesheets   0.1.0.9000         0.1.0 FALSE          1
# 207     practice        0.1.0            NA FALSE          1
# 236       reprex   0.0.0.9001            NA FALSE          1
# 303        tidyr   0.3.1.9000         0.3.1 FALSE          1

install_me <- subset(prev_pkgs, subset = !ok, select = Package)

gh_usernames <- c("argufy" = "gaborcsardi",
                  "devtools" = "hadley",
                  "dplyr" = "hadley",
                  "EDAWR" = "rstudio",
                  "ggplot2" = "hadley",
                  "gh" = "gaborcsardi",
                  "googlesheets" = "jennybc",
                  "practice" = "ironholds",
                  "reprex" = "jennybc",
                  "tidyr" = "hadley")
install_me$gh_username <- I(gh_usernames[install_me$Package])
install_me$repo <- paste(install_me$gh_username, install_me$Package, sep = "/")
res <- devtools::install_github(install_me$repo)

## are we done?
my_pkgs <- data.frame(installed.packages(lib.loc = .libPaths()[1]))[c('Package', 'Version')]
my_pkgs$Package <- as.character(my_pkgs$Package)
my_pkgs$Version <- as.character(my_pkgs$Version)

prev_pkgs$fresh_version <- my_pkgs$Version[match(prev_pkgs$Package, my_pkgs$Package)]
prev_pkgs$comparison <- vcv(prev_pkgs$prev_version, prev_pkgs$fresh_version)
prev_pkgs$ok <- prev_pkgs$comparison <= 0

table(prev_pkgs$ok)
# TRUE 
# 326 

update.packages(lib.loc = "/Users/jenny/resources/R/library", dependencies = TRUE)
## the beautiful sound of silence ...
