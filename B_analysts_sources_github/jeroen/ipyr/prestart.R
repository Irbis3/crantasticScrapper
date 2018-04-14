#standard libraries
oldlibs <- environment(.libPaths)$.lib.loc

#add the ipyr library
environment(.libPaths)$.lib.loc <- c("/usr/lib/ipyr/library", oldlibs)

#load ipyr
getNamespace("IRkernel");

#remove the ipyr library
environment(.libPaths)$.lib.loc <- oldlibs

