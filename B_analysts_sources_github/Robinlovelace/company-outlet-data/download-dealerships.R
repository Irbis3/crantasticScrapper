# Aim: scrape data on toyota dealerships
pkgs = c("rvest", "dplyr", "sp", "mapview", "tmap", "readr")
lapply(pkgs, library, character.only = T)

u = "http://www.toyota-garages.co.uk/toyota/" # url
h = read_html(u)

dealers = data_frame(
  Name = h %>% 
    html_nodes(css = ".dealername a") %>% 
    html_text(),
  Postcode = h %>% 
    html_nodes(css = ".dealerpostcode") %>% 
    html_text()
)

# generalise for all sites

url_numbers = seq(from = 0, to = 210, by = 20)[-1]
i = 20
dealers_n = as.list(url_numbers)
names(dealers_n) = url_numbers
for(i in url_numbers){
  u = "http://www.toyota-garages.co.uk/toyota/index.php?dealer="
  u_n = paste0(u, i)
  h = read_html(u_n)
  dealers_n[[as.character(i)]] = data_frame(
    Name = h %>% 
      html_nodes(css = ".dealername a") %>% 
      html_text(),
    Postcode = h %>% 
      html_nodes(css = ".dealerpostcode") %>% 
      html_text()
  )
}

all_dealers = bind_rows(dealers_n)
all_dealers = bind_rows(dealers, all_dealers)

# geocode results
geo_locations = ggmap::geocode(all_dealers$Postcode)
all_dealers = cbind(all_dealers, geo_locations)
dealers_sp = SpatialPointsDataFrame(coords = as.matrix(geo_locations), data = all_dealers)
bbox(dealers_sp)
dealers_sp = dealers_sp[dealers_sp$lon < 10,]
bbox(dealers_sp)
mapview(dealers_sp)

# check results
osm_tiles = read_osm(bb(dealers_sp))
qtm(osm_tiles) +
  tm_shape(dealers_sp) + tm_dots()
geojsonio::geojson_write(dealers_sp, file = "C:/Users/georl/Desktop/GitHub/company-outlet-data/output-data/dealers.geojson")
dealers_sp = geojsonio::geojson_read("output-data/dealers.geojson", what = "sp")

plot(dealers_sp)

la_data = read_csv("D://tmp/integrated_la_congestion.csv")
nrow(la_data)

url_la = "https://geoportal.statistics.gov.uk/Docs/Boundaries/Local_authority_district_(GB)_2011_Boundaries_(Generalised_Clipped).zip"
download.file(url_la, destfile = "data_la.zip")
unzip("data_la.zip")
las = read_shape("LAD_DEC_2011_GB_BGC.shp")
plot(las)
bbox(las)
las = spTransform(las, CRSobj = proj4string(dealers_sp))

dealers_ag = aggregate(dealers_sp["Name"], las, length)
summary(dealers_ag@data)
qtm(dealers_ag, "Name")
nrow(dealers_ag)
nrow(las)
plot(dealers_ag[88,])
plot(las[88,])
las$n_dealers = dealers_ag$Name

# merge csv - find shared variables
head(las$LAD11CD)
las@data = data.frame(
  GeoCode = las$LAD11CD,
  n_dealers = las$n_dealers
)
head(la_data$GeoCode)

la_data = left_join(la_data, las@data)
write_csv(la_data["n_dealers"], "output-data/n_dealer.csv")
