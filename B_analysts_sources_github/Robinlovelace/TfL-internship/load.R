#Load zone and OD data
library(sf)
z = readRDS("cyoddata-master/z_tfl.Rds")
plot(z$geometry)
