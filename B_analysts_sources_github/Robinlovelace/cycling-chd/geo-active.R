# Aim: get geographical data on active travel over time
library(tmap)
library(dplyr)
library(sp)
# data from https://github.com/npct/pct-bigdata
msoas = read_shape("~/npct/pct-bigdata/msoasmapshaped_25%.shp")
# qtm(msoas)

# merge in travel data
msoas_age_mode = readr::read_csv("data/msoas-age-mode.csv")
names(msoas_age_mode) = gsub("Method of travel to work \\(2001 specification\\): ", "", names(msoas_age_mode))
names(msoas_age_mode) = gsub("Method of travel to work \\(2001 specification\\);", "", names(msoas_age_mode))
names(msoas_age_mode) = gsub("; measures: Value", "", names(msoas_age_mode))
names(msoas_age_mode) = gsub(" categories:  Age:", "", names(msoas_age_mode))
names(msoas_age_mode) = gsub(" All categories: Age 16 to 74", "", names(msoas_age_mode))
names(msoas_age_mode) = gsub(" Age ", "_", names(msoas_age_mode))
names(msoas_age_mode) = gsub(" to ", "_", names(msoas_age_mode))
names(msoas_age_mode) = gsub("; Age:", "", names(msoas_age_mode))

# sort out the modes
names(msoas_age_mode) = gsub("All other methods of travel_work", "other", names(msoas_age_mode))
names(msoas_age_mode) = gsub("On foot", "foot", names(msoas_age_mode))
names(msoas_age_mode) = gsub("Driving a car or van", "drive", names(msoas_age_mode))
names(msoas_age_mode) = gsub("Train, underground, metro, light rail, tram, bus, minibus or coach", "public", names(msoas_age_mode))
names(msoas_age_mode) = gsub("Work mainly at or from home", "home", names(msoas_age_mode))
names(msoas_age_mode) = tolower(names(msoas_age_mode))

# write.csv(msoas_age_mode, "data/msoas-age-mode.csv")

head(msoas@data)
head(msoas_age_mode[1:4])
names(msoas_age_mode)[4] = "geo_code"

merged_data = left_join(msoas@data, msoas_age_mode)
msoas@data = merged_data

# geojsonio::geojson_write(msoas, file = "data/msoas-age-mode.geojson")
msoas = geojsonio::geojson_read("data/msoas-age-mode.geojson", what = "sp")

head(msoas@data)
library(tidyr)
?gather
mlong = gather(msoas@data[-c(2:7)], key = age_sex, value = Count, -geo_code)
library(dplyr)
mlong = filter(mlong, !grepl(pattern = "all", age_sex))
mlong = mlong[grep(pattern = "[0-9]", x = mlong$age_sex),]
head(mlong)

plot(msoas)
summary(msoas$bicycle_40_44)
m = qtm(msoas, "bicycle_40_44", fill.style = "quantile", borders = "NA")
dir.create("figures")
save_tmap(m, "figures/cycle-40-44.png")
