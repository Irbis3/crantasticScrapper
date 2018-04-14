# dependencies
library("curl")
library("readxl")
library("dplyr")

# download
url <- "http://datadryad.org/bitstream/handle/10255/dryad.235/GlobalWoodDensityDatabase.xls?sequence=1"
curl::curl_download(url, destfile = "chave.xls")

# read in data
dat <- readxl::read_excel("chave.xls", sheet = "Data")

# clean data
## neat column headers
dat <- dplyr::rename(dat,
              no = Number,
              family = Family,
              binomial = Binomial,
              density = `Wood density (g/cm^3), oven dry mass/fresh volume`,
              region = Region,
              ref_no = `Reference Number`)
## remove NA's - only 1
dat <- dplyr::filter(dat, !is.na(density))

# write clean data
write.csv(dat, file = "chave.csv")

# cleanup
unlink("chave.xls")
