########################################################
## Author: Diego Valle-Jones
## Website: www.diegovalle.net
## Date Created: Fri Sep  7 18:02:08 2012
## Email: diegovalle at gmail.com
## Purpose: Clean and Merge Map Files of Mexico and Central American Countries 
## Copyright (c) Diego Valle-Jones. All rights reserved

#Theme to get rid of ggplot's axis and panels
theme_nothing <- function() {
  theme_bw() + theme(axis.text.x = element_blank(),
                     axis.title.x = element_blank(),
                     axis.text.y = element_blank(),
                     axis.title.y = element_blank(),
                     axis.line = element_blank(), axis.ticks = element_blank(),
                     panel.background = element_blank(), 
                     panel.border = element_blank(), 
                     panel.grid.major = element_blank(), 
                     panel.grid.minor = element_blank(), 
                     plot.background = element_blank()) 
}


getMaps <- function(codes, level) {
  ##Different column names to uniquely identify each group
  ##depending on the level requestes
  column.name <- ifelse(level == 1, "NAME_0", "ISO")
  ##Download the maps
  country.ll <- llply(codes,
                      function(x) getData("GADM", path = "maps",
                                          country = x, level = level))
  ##Change the id of the maps since some are repeated and we need to
  ##merge them, use ISO code as the unique identifier
  country.ll <- llply(country.ll,
                      function(x) spChFIDs(x, str_c(row.names(x), x[[column.name]][1])))
  ##Merge the list of maps one by one
  for(i in 2:length(country.ll)){
    if(i == 2) {
      map <- spRbind(country.ll[[1]], country.ll[[2]])
    } else { 
      map <- spRbind(map, country.ll[[i]])
    }
  }
  map
}
##Codes for the Central American Countries
country.codes <- c("GTM", "BLZ", "HND", "SLV", "CRI", "NIC", "PAN")
##Get the maps of States/Districts
map <- getMaps(country.codes, level = 1)
map$NAME_1 <- iconv(map$NAME_1, from = "latin1", to = "UTF-8")

##Get the maps of the country outlines
map.borders <- getMaps(country.codes, level = 0)

##plot(map)


##Add the map of Mexican municipalities to the map of Central America

##Make sure we are working with the same projection
mx <- spTransform(mx, CRS(proj4string(map)))
##Only the states bordering Guatemala and Belize:
##Chiapas, Tabasco, Campeche, Quintana Roo
mx <- mx[mx$CVE_ENT %in% c("07", "27", "04", "23"), ]
##Subset the data.frame that comes with the map to be
##able to merge the two maps
map <- map[, "NAME_1"]
mx$NAME_1 <- as.numeric(str_c(mx$CVE_ENT, mx$CVE_MUN))
mx <- mx[,"NAME_1"]

##Merge map of Central America with the Mexican municipalities
map <- spRbind(map, mx)

##Get rid of the Panama regions I wasn't able to merge with the map
map <- map[!map$NAME_1 %in% c("Emberá", "Kuna Yala", "Ngöbe Buglé", "Nicaragua"), ]

##Map of Mexican States
mx.states <- spTransform(mx.states, CRS(proj4string(map)))
mx.states <- mx.states[mx.states$CVE_ENT %in% c("07", "27", "04", "23"), ]

##Prepare for ggplt
ca.map <- fortify(map, region = "NAME_1")
ca.map.borders <- fortify(map.borders, region = "NAME_ISO")
states.map.borders <- fortify(mx.states, region = "NOM_ENT")

##Merge with the homicide data
ca.map2 <- join(ca.map, ca, by = "id")
##Are any regions missing from the join?
##unique(ca.map2[is.na(ca.map2$rate),]$id)

##Add police homicide info to the state map
states.map.borders <- join(states.map.borders, snsp)
