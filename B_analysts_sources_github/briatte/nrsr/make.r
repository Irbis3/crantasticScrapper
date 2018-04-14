# hi Slovakia

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

dir.create("raw", showWarnings = FALSE)

# parameters

plot = TRUE
gexf = TRUE
mode = "fruchtermanreingold"
meta = c(
  "cty" = "Slovakia",
  "lang" = "sk", # Wikipedia language for chamber and constituencies
  "ch" = "Národná_rada_Slovenskej_republiky",
  "type" = "Unicameral",
  "ipu" = 2285,
  "seats" = 150
)

# build routine

source("data.r")  # scrape bills and sponsors
source("build.r") # assemble the networks
source("comm.r")  # add committee co-membership

save(list = ls(pattern = "^(co)?(net|edges|bills)_sk\\d{4}$"),
     file = "data/net_sk.rda")

# have a nice day
