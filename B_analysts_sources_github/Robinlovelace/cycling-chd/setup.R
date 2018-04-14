# Aim load-in the data
pkgs <- c("readxl", "stplanr", "raster", "rgdal", "dplyr", "downloader", "tmap")
# install.packages(pkgs) uncomment to install these packages
lapply(pkgs, library, character.only = TRUE)
