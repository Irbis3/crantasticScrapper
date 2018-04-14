# hi Czech Republic

source("load.r")
source("functions.r")
source("parties.r")

# folders

dir.create("data"  , showWarnings = FALSE)
dir.create("plots" , showWarnings = FALSE)

if (file.exists("photos_po.zip"))
  unzip("photos_po.zip")

if (file.exists("photos_se.zip"))
  unzip("photos_se.zip")

dir.create("photos_po", showWarnings = FALSE)
dir.create("photos_se", showWarnings = FALSE)

if (file.exists("raw.zip"))
  unzip("raw.zip")

dir.create("raw"               , showWarnings = FALSE)
dir.create("raw/po"            , showWarnings = FALSE)
dir.create("raw/po/bill-lists" , showWarnings = FALSE)
dir.create("raw/po/bill-pages" , showWarnings = FALSE)
dir.create("raw/po/mp-pages"   , showWarnings = FALSE)
dir.create("raw/se"            , showWarnings = FALSE)
dir.create("raw/se/constituencies" , showWarnings = FALSE)
dir.create("raw/se/bill-pages"     , showWarnings = FALSE)
dir.create("raw/se/senator-pages"  , showWarnings = FALSE)

# parameters

plot = TRUE
gexf = TRUE
mode = "fruchtermanreingold"
meta = c(
  "cty" = "Czech Republic",
  "lang" = "cs", # Wikipedia language for chamber and constituencies
  "ch-po" = "Poslanecká_sněmovna_Parlamentu_České_republiky",
  "ch-se" = "Senát_Parlamentu_České_republiky",
  "type-po" = "Lower",
  "type-se" = "Upper",
  "ipu-po" = 2083,
  "ipu-se" = 2084,
  "seats-po" = 200,
  "seats-se" = 81
)

yrs = c("1" = "1992-1996", "2" = "1996-1998", "3" = "1998-2002",
        "4" = "2002-2006", "5" = "2006-2010", "6" = "2010-2013",
        "7" = "2013-2017")

# build routine (1): lower chamber

source("data-po.r")  # scrape bills and sponsors
source("build-po.r") # assemble the networks
source("comm-po.r")  # add committee co-membership

# build routine (2): upper chamber

source("data-se.r")  # scrape bills and sponsors
source("build-se.r") # assemble the networks
source("comm-se.r")  # add committee co-membership

save(list = ls(pattern = "^(co)?(net|edges|bills)_cz_(po|se)\\d{4}$"),
     file = "data/net_cz.rda")

# have a nice day
