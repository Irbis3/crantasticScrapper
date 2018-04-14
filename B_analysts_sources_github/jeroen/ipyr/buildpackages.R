#Remove local and home libraries to make sure we only depends on base and recommended packages.
assign(".lib.loc", c("/usr/lib/R/library"), envir=environment(.libPaths));

#this dir contains the source packages
sourcedir <- file.path(getwd(), "lib")
destdir <- file.path(sourcedir, "build");
stopifnot(dir.create(destdir))

#we fist need to create a package index
library(tools);
write_PACKAGES(sourcedir)

#Dependencies are automatically installed as well.
install.packages(c("RAppArmor", "IRkernel"), type="source", lib=destdir, contriburl=paste0("file://", sourcedir));
