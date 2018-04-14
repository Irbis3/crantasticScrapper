# Save your from Strava and save as activities.zip in the data_raw folder
# See https://support.strava.com/hc/en-us/articles/216918437-Exporting-your-Data-and-Bulk-Export#Bulk

source("setup.R")

# unzip gpx files from strava
unzip(zipfile = "data_raw/activities.zip", exdir = "data_gpx/")
gps_files = list.files(path = "data_gpx/", full.names = TRUE)

# load one gpx file
f = gps_files[1]
st_layers(f)
track = st_read(dsn = f, layer = "tracks")
p = st_read(f, "track_points")
mapview(track) + mapview(p)
track_sp = as(object = track, Class = "Spatial")
track_tt = stplanr::toptail(track_sp, toptail_dist = 1000)
p_sp = as(p, "Spatial")
p_tt = p_sp[track_tt,]
mapview(track_tt) # topntailed version
mapview(p_tt)

saveRDS(track_tt, "data/track_tt.Rds")
saveRDS(p_tt, "data/p_tt.Rds")

# iterate over all files ...
