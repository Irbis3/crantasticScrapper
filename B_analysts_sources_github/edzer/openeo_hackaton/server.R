library(plumber)
o = plumb("openeo.R")
o$run(port=8000)
