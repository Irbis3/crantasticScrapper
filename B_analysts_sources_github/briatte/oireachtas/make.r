# hi Ireland

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
dir.create("raw/mp-lists"   , showWarnings = FALSE)
dir.create("raw/mp-pages"   , showWarnings = FALSE)
dir.create("raw/committees" , showWarnings = FALSE)

# parameters

plot = TRUE
gexf = TRUE
mode = "fruchtermanreingold"
meta = c(
  "cty" = "Ireland",
  "lang" = "en", # Wikipedia language for chamber and constituencies
  "da" = "Dáil Éireann",
  "se" = "Seanad Éireann",
  "type-da" = "Lower",
  "type-se" = "Upper",
  "ipu-da" = 2153,
  "ipu-se" = 2154,
  "seats-da" = 166,
  "seats-se" = 60
)

# build routine

source("data.r")  # scrape bills and sponsors
source("build.r") # assemble the networks

# do not add committee co-memberships: committees are available only for the
# current house, and there are too few cosponsorship ties to create weighted
# co-membership ties from that

# source("comm.r")  # add committee co-membership

save(list = ls(pattern = "^(co)?(net|edges|bills)_ie_(da|se)\\d{4}$"),
     file = "data/net_ie.rda")

# have a nice day
