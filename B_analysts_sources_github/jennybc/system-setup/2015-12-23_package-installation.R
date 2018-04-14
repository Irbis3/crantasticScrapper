library(dplyr)
library(readr)

### installed binary of ...
### R version 3.2.3 (2015-12-10) -- "Wooden Christmas-Tree"
### Copyright (C) 2015 The R Foundation for Statistical Computing
### Platform: x86_64-apple-darwin13.4.0 (64-bit)

### does anything already need update?
update.packages(lib.loc = .Library)
## mgcv updated

## check if anything *other* than base and recommended packages are
## in the standard library ... I hate that
std_lib_pkgs <- data.frame(installed.packages(lib.loc = .Library))
all(unique(std_lib_pkgs$Priority) %in% c('base', 'recommended'))
## TRUE

## currently installed packages?
curr_pkgs <- installed.packages(lib.loc = .libPaths()[1]) %>%
  tbl_df()
curr_pkgs
curr_pkgs %>%
  count(Built)
write_csv(curr_pkgs %>%
            select(Package, Version),
          "~/resources/system-setup/2015-12-23_packages-before-upgrade.csv")

update.packages(lib.loc = .libPaths()[1], dependencies = TRUE)
## authorized several pkgs to be installed from source

## error from building spdep
# ld: warning: directory not found for option '-L/usr/local/lib/gcc/x86_64-apple-darwin13.0.0/4.8.2'
# ld: library not found for -lgfortran
# clang: error: linker command failed with exit code 1 (use -v to see invocation)
# make: *** [spdep.so] Error 1
# ERROR: compilation failed for package ‘spdep’
# * removing ‘/Users/jenny/resources/R/library/spdep’

## same error for openair

## errors (?) for raster
# Creating a generic function for ‘as.vector’ from package ‘base’ in package ‘raster’
# in method for ‘brick’ with signature ‘x="big.matrix"’: no definition for class “big.matrix”
# in method for ‘brick’ with signature ‘x="kasc"’: no definition for class “kasc”
# in method for ‘brick’ with signature ‘x="grf"’: no definition for class “grf”
# in method for ‘coerce’ with signature ‘"STFDF","RasterBrick"’: no definition for class “STFDF”
# in method for ‘coerce’ with signature ‘"STSDF","RasterBrick"’: no definition for class “STSDF”
# in method for ‘coerce’ with signature ‘"asc","RasterLayer"’: no definition for class “asc”
# in method for ‘coerce’ with signature ‘"RasterLayer","asc"’: no definition for class “asc”
# in method for ‘coerce’ with signature ‘"kasc","RasterBrick"’: no definition for class “kasc”
# in method for ‘coerce’ with signature ‘"kasc","RasterStack"’: no definition for class “kasc”
# in method for ‘coerce’ with signature ‘"kde","RasterLayer"’: no definition for class “kde”
# in method for ‘coerce’ with signature ‘"grf","RasterBrick"’: no definition for class “grf”
# in method for ‘coerce’ with signature ‘"grf","RasterLayer"’: no definition for class “grf”
# Creating a generic function for ‘scale’ from package ‘base’ in package ‘raster’
# in method for ‘stack’ with signature ‘x="kasc"’: no definition for class “kasc”
## though package installation seems to have been successful?

## rgdal
# configure: error: gdal-config not found or not executable.
# ERROR: configuration failed for package ‘rgdal’
# * removing ‘/Users/jenny/resources/R/library/rgdal’

## ggobi
# checking for pkg-config... no
# checking for GGOBI... no
# configure: error: in `/private/var/folders/vt/4sdxy0rd1b3b65nqssx4sx_h0000gn/T/RtmppqyiIQ/R.INSTALL13992da54877/rggobi':
# configure: error: The pkg-config script could not be found or is too old.  Make sure it
# is in your PATH or set the PKG_CONFIG environment variable to the full
# path to pkg-config.
#
# Alternatively, you may set the environment variables GGOBI_CFLAGS
# and GGOBI_LIBS to avoid the need to call pkg-config.
# See the pkg-config man page for more details.
#
# To get pkg-config, see <http://pkg-config.freedesktop.org/>.
# See `config.log' for more details
# ERROR: configuration failed for package ‘rggobi’
# * removing ‘/Users/jenny/resources/R/library/rggobi’

## rms
# gfortran-4.8   -fPIC  -g -O2  -c lrmfit.f -o lrmfit.o
# make: gfortran-4.8: No such file or directory
# make: *** [lrmfit.o] Error 1
# ERROR: compilation failed for package ‘rms’
# * removing ‘/Users/jenny/resources/R/library/rms’

## https://stat.ethz.ch/pipermail/r-sig-mac/2015-October/011663.html
## https://cran.r-project.org/doc/manuals/r-patched/R-admin.html#OS-X
