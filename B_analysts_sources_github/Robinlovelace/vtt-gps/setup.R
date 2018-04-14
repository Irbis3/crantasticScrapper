# load libraries
pkgs = c("rgdal", "tmap", "raster", "mapview", "osmplotr", "rgeos", "stplanr")
# install.packages(pkgs) # to get all the packages we're using for this
lapply(pkgs, library, character.only = T)


