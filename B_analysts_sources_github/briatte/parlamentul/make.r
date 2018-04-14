# hi Romania

source("load.r")
source("functions.r")
source("parties.r")

# folders

dir.create("data"  , showWarnings = FALSE)
dir.create("plots" , showWarnings = FALSE)

if (file.exists("photos.zip"))
  unzip("photos.zip")

dir.create("photos", showWarnings = FALSE)

if (file.exists("raw.zip"))
  unzip("raw.zip")

dir.create("raw"            , showWarnings = FALSE)
dir.create("raw/bill-lists" , showWarnings = FALSE)
dir.create("raw/bill-pages" , showWarnings = FALSE)
dir.create("raw/mp-pages"   , showWarnings = FALSE)

# parameters

plot = TRUE
gexf = TRUE
mode = "fruchtermanreingold"
meta = c(
  "cty" = "Romania",
  "lang" = "en", # Wikipedia language for chamber and constituencies
  "ca" = "Chamber_of_Deputies_(Romania)",
  "se" = "Senate_of_Romania",
  "type-ca" = "Lower",
  "type-se" = "Upper",
  "ipu-ca" = 2261,
  "ipu-se" = 2262,
  "seats-ca" = 412,
  "seats-se" = 176
)

# build routine

source("data.r")  # scrape bills and sponsors
source("build.r") # assemble the networks
source("comm.r")  # add committee co-membership

save(list = ls(pattern = "^(co)?(net|edges|bills)_ro_(ca|se)\\d{4}$"),
     file = "data/net_ro.rda")

# have a nice day
