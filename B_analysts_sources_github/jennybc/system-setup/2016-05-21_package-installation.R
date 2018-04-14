library(dplyr)
library(readr)

### installed binary of ...
### R version 3.3.0 (2016-05-03) -- "Supposedly Educational"
### Copyright (C) 2016 The R Foundation for Statistical Computing
### Platform: x86_64-apple-darwin13.4.0 (64-bit)

### does anything already need update?
update.packages(lib.loc = .Library)
## nlme, survival updated

## check if anything *other* than base and recommended packages are
## in the standard library ... I hate that
std_lib_pkgs <- as_data_frame(installed.packages(lib.loc = .Library))
all(unique(std_lib_pkgs$Priority) %in% c('base', 'recommended'))
## TRUE

## currently installed packages?
curr_pkgs <- installed.packages(lib.loc = .libPaths()[1]) %>%
  tbl_df()
curr_pkgs
curr_pkgs %>%
  count(Built)
#Source: local data frame [5 x 2]
#
#  Built     n
#  <chr> <int>
#1 3.2.0   351
#2 3.2.2    31
#3 3.2.3    83
#4 3.2.4    13
#5 3.2.5     5
write_csv(curr_pkgs %>%
            select(Package, Version),
          "~/resources/system-setup/2016-05-21_packages-before-upgrade.csv")

update.packages(lib.loc = .libPaths()[1], dependencies = TRUE)

# as usual, Rmpi and rggobi won't just install
