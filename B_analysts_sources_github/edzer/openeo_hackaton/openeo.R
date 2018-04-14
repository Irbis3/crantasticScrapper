read_band = function(band) {
	stopifnot(is.character(band) && length(band) == 1)
	bnd = strsplit(band, "/")[[1]]
	if (!file.exists(paste0("openeo_data", bnd[3]))) {
		cat(paste0("unzipping ", bnd[3], "\n"))
		cmd = paste("(cd openeo_data; unzip -n ", bnd[3], ")")
		system(cmd)
	}
	if (bnd[4] == "red")
		b = 3
	else if (bnd[4] == "green")
		b = 2
	else
		stop("unknown band")
	require(rgdal)
	fname = paste0("openeo_data/", bnd[3], ".SAFE/MTD_MSIL1C.xml")
	system(paste("gdalinfo", fname, "| grep \"SUBDATASET_1_NAME\" > fileinfo"))
	f = file("fileinfo")
	r = strsplit(readLines(f), "=")[[1]][2]
	close(f)
	readGDAL(r, band = b)
}

write_band = function(band, name) {
	nm = strsplit(name, "/")[[1]]
	fname = paste0("openeo_data/", nm[4])
	writeGDAL(band, fname)
}

#* @post /process/addition
addTwoBands <- function(band1, band2, output){
  a = read_band(band1)
  b = read_band(band2)
  a$out = a$band1 + b$band1
  a$band1 = NULL
  write_band(a, output)
  output
}

#* @get /data/user
fetch_band = function(name, format) {
	fname = paste0("openeo_data/", name)
	fname
}

band1 = "/data/S2A_MSIL1C_20170210T100131_N0204_R122_T33UVP_20170210T100132/red" # band 4
band2 = "/data/S2A_MSIL1C_20170210T100131_N0204_R122_T33UVP_20170210T100132/green" # band 3
out = addTwoBands(band1, band2, "/data/user/foo")
summary(out)
