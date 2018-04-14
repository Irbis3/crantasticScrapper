library(raster)
library(leaflet) # for viewing
system.time({r = stack("TMO_georef.tif")})
r # 8000 by 5000

msg = "gdal2tiles.py TMO_georef_4326.tif"
system.time(system(msg)) # 50 seconds
dim(r)[1] * dim(r)[2] / 50000 # 761,000 pixels/second on 3 bands
file.edit("TMO_georef_4326/leaflet.html") # works

dir.create("partiles")
msg = "python ~/other-repos/gdal2tiles_parallel.py -p mercator TMO_georef_4326.tif partiles/"
system.time(system(msg)) # 13 seconds - ~ 4 times faster...
file.copy("TMO_georef_4326/leaflet.html", "partiles/leaflet.html")
file.edit("partiles/leaflet.html")
