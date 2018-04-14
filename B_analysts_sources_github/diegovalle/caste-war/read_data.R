## Read the listados nominales of the PAN, PRI, PRD

## use skip=4 to skip blank lines
pan <- read.csv(file.path("INE", "1_PAN.xlsx.csv"), skip = 4)
pan$party <- "PAN"

prd <- read.csv(file.path("INE", "PRD", "3_PRD Estados 1 al 12.xlsx.csv"), skip = 3)
prd  <- rbind(prd,
              read.csv(file.path("INE", "PRD", "3_PRD Estados 13 al 19.xlsx.csv"), skip = 3))
prd  <- rbind(prd,
              read.csv(file.path("INE", "PRD", "3_PRD Estados 20 al 32.xlsx.csv"), skip = 3))
prd$party <- "PRD"

pri <- read.csv(file.path("INE", "PRI", "2_PRI Estados 1 al 7.xlsx.csv"), skip = 3)
pri  <- rbind(pri,
              read.csv(file.path("INE", "PRI", "2_PRI Estados 8 al 13.xlsx.csv"), skip = 3))
pri  <- rbind(pri,
              read.csv(file.path("INE", "PRI", "2_PRI Estados 14 al 15.xlsx.csv"), skip = 3))
pri  <- rbind(pri,
              read.csv(file.path("INE", "PRI", "2_PRI Estados 16.xlsx.csv"), skip = 3))
pri  <- rbind(pri,
              read.csv(file.path("INE", "PRI", "2_PRI Estados 17 al 19.xlsx.csv"), skip = 3))
pri  <- rbind(pri,
              read.csv(file.path("INE", "PRI", "2_PRI Estados 20 al 24.xlsx.csv"), skip = 3))
pri  <- rbind(pri,
              read.csv(file.path("INE", "PRI", "2_PRI Estados 25 al 32.xlsx.csv"), skip = 3))
pri$party <- "PRI"

party.names <- c("entidad", "paterno", "materno", "nombre", 
                 "afiliacion", "partido")
names(pri) <- party.names
names(prd) <- party.names
names(pan) <- party.names

replaceSpace <- function(df) {
  df$paterno <- str_replace_all(df$paterno, "\\s","_")
  df$materno <- str_replace_all(df$materno, "\\s","_")
  return(df)
}
pri <- replaceSpace(pri)
pan <- replaceSpace(pan)
prd <- replaceSpace(prd)
#sourceCpp("sort.cpp")

all(unique(prd$entidad)== unique(pan$entidad))
all(unique(prd$entidad)== unique(prd$entidad))
codes <- data_frame(entidad = unique(prd$entidad), state_code = 1:32)
map = setNames(codes$state_code, codes$entidad)

pan$state_code <- map[unlist(pan$entidad)]
prd$state_code <- map[unlist(prd$entidad)]
pri$state_code <- map[unlist(pri$entidad)]

all <- rbind(pri,pan,prd)
rm(pri);rm(pan);rm(prd)


all$nombre <- str_replace_all(all$nombre, "^J | J | J$", " JUAN ")
all$nombre <- str_replace_all(all$nombre, "^MA | MA | MA$", " MARIA ")
all$nombre <- str_replace_all(all$nombre, "^M | M | M$", " MARIA ")
all$nombre <- str_replace_all(all$nombre, "^MA. | MA. | MA.$", " MARIA ")
all$nombre <- str_replace_all(all$nombre, "^S C | S C | S C$", " SAGRADO CORAZON ")
all$nombre <- str_replace_all(all$nombre, "^SDO | SDO | SDO$", " SAGRADO ")
all$nombre <- str_replace_all(all$nombre, "^SGDO | SGDO | SGDO$", " SAGRADO  ")
all$nombre <- str_replace_all(all$nombre, "^M | M | M$", "MARIA ")

all$nombre <- toupper(all$nombre)

## Read the state map

states <- readOGR("maps", "ESTADOS")
bb <- bbox(as(extent(states) , "SpatialPolygons" ) )
states.ff <- fortify(states, region='CVE_ENT')
states.ff$state_code <- as.numeric(states.ff$id)

state_centroids <- as.data.frame(coordinates(states))
names(state_centroids ) <- c("long", "lat")
state_centroids$state_code <- 1:32


PAN.COLOR <- "#2b8cbe"
PRI.COLOR <- "#de2d26"
PRD.COLOR <- "#fed300"
