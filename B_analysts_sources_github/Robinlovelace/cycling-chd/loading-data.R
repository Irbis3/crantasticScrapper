# Aim load-in the data
pkgs <- c("readxl", "stplanr", "raster", "rgdal")
# install.packages(pkgs) uncomment to install these packages
lapply(pkgs, library, character.only = TRUE)

# available from here: http://fingertips.phe.org.uk/profile/health-profiles/data#gid/1938132694/pat/6/ati/101/page/9/par/E12000004/are/E07000032
df <- read_excel("data/PublicHealthEngland-Data.xls", sheet = 3)

# # load la boundaries, from http://data.gov.uk/dataset/county-and-unitary-authorities-ew-2012-boundaries-full-extent
# url <- "County_and_unitary_authorities_(E+W)_2012_Boundaries_(Full_Extent).zip"
# unzip(zipfile = url, exdir = "data/")
# gMapshape(dsn = "data/CTYUA_DEC_2012_EW_BFE.shp", percent = 1)
# las <- shapefile("data/CTYUA_DEC_2012_EW_BFEmapshaped_1%.shp")
# las <- spTransform(las, CRSobj = CRS("+init=epsg:4326"))
# library(geojsonio) # need devtools and then install_github("ropensci/geojsonio")
# geojson_write(input = las, file = "data/las.geojson")

# las <- readOGR(dsn = "data/las.geojson", layer = "OGRGeoJSON")
# nrow(las)
# plot(las) # wrong geometry

# load correct las geometry
# url <- "https://github.com/npct/pct-bigdata/raw/master/national/las-pcycle.geojson"
# downloader::download(url, destfile = "data/las-pcycle.geojson")
las <- readOGR(dsn = "data/las-pcycle.geojson", layer = "OGRGeoJSON")
