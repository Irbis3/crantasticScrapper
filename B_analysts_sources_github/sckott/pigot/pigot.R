# dependencies
library("curl")
library("readxl")
library("dplyr")

# download
url <- "http://datadryad.org/bitstream/handle/10255/dryad.99897/Database%20S1%20Pigot%2c%20Trisos%20and%20Tobias.xls?sequence=1"
curl::curl_download(url, destfile = "pigot.xls")

# read in data
dat <- readxl::read_excel("pigot.xls", sheet = "Database S1")

# clean data
## neat column headers
### all lowercase
names(dat) <- tolower(names(dat))
### trim whitespace
names(dat) <- gsub("^\\s+|\\s+$", "", names(dat))
### replace whitespace in middle of names
names(dat) <- gsub("\\s", "_", names(dat))

## remove NA's - only 1
dat <- dplyr::filter(dat, !is.na(density))

# write clean data
write.csv(dat, file = "pigot.csv", row.names = FALSE)

# cleanup
unlink("pigot.xls")
