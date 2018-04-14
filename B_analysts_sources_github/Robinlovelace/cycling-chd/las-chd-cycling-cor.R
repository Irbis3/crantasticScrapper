source("setup.R")
f <- read_excel("data/PublicHealthEngland-Data.xls", sheet = 3)
head(f)
unique(f$Indicator)

chd <- f[f$Indicator == "Under 75 mortality rate: cardiovascular",]
chd <- summarise(group_by(chd, `Area Code`), chd_rate = mean(Value))

# Load las
old <- setwd("~/repos/pct/")
source("~/repos/pct/loading-data/load-sex.R")
setwd(old)

qtm(las, "clc")

chd <- rename(chd, CODE = `Area Code`)

# chd <- inner_join(chd, pcycle)
head(chd)

head(las@data)

las@data <- left_join(las@data, chd)
head(las@data)

tmap::qtm(las, fill = c("chd_rate", "log_pcycle"))
plot(las$log_pcycle, las$chd_rate, xlab = "Log of % cycling",
  ylab = "Cardio mortality rate")
cor(las$clc * 100, las$chd_rate, use = "complete.obs")






# # Seems to be failing: a bug with geojson files?
# las <- readOGR(dsn = "data/las-pcycle.geojson", layer = "OGRGeoJSON")
# las <- geojsonio::geojson_read("data/las-pcycle.geojson")
# las <- geojsonio::as.SpatialPolygonsDataFrame(las)
# qtm(las, "clc")
