# Load gender equality
source("set-up.R")

library(downloader)
# download("http://census.edina.ac.uk/ukborders/easy_download/prebuilt/shape/England_lad_2011_gen_clipped.tar.gz", "bigdata/England_lad_2011_gen_clipped.tar.gz")
# untar(tarfile = "bigdata/England_lad_2011_gen_clipped.tar.gz", exdir = "bigdata/")
# gMapshape(dsn = "bigdata/England_lad_2011_gen_clipped.shp", 1)
las <- shapefile("bigdata/England_lad_2011_gen_clippedmapshaped_1%.shp")
plot(las)
las@data <- rename(las@data, GeographyCode = CODE)

las$GeographyCode <- as.character(las$GeographyCode)

head(las)
library(readr)

# link geographical zones to data
# https://wicid.ukdataservice.ac.uk/cider/info.php?geogtype=96&lablist=1
linkla <- read_csv("pct-bigdata/national/la-old-new.csv")
head(linkla)
linkla <- rename(linkla, GeographyCode = ONS)
df <- read_csv("bigdata/DC7101EWla_2011CMLADH_NAT_LAD_REL_1.1.1_20140228-1007-06168/DC7101EWla_2011CMLADH_NAT_LAD_REL_1.1.1/DC7101EWlaDATAA5.CSV")

head(df[1:4])
df <- left_join(df, linkla)
head(df[250:256])

summary(df$clc <- df$DC7101EWla0007 / df$DC7101EWla0001)
summary(df$clc_m <- df$DC7101EWla0124 / df$DC7101EWla0007)

df <- dplyr::select(df, CODE, GeographyCode, clc, clc_m)
head(df)
head(las$GeographyCode)
head(df$CODE) # they are the same

summary(las$GeographyCode %in% df$CODE)
las@data <- rename(las@data, CODE = GeographyCode)

head(las$CODE)
head(df$CODE)
las@data <- left_join(las@data, df, by = "CODE")

# qtm(las, "clc") # test the map makes sense
las@data[ which(las$clc > 0.1), ]
las$log_pcycle <- log(las$clc * 100)
library(tmap)
tmap::qtm(shp = las, "log_pcycle")
