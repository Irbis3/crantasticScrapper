library(rfisheries)
library(plyr)
library(reshape2)
library(ggplot2)

species <- of_species_codes()
who <- c("TUX", "COD", "VET", "NPA")
by_species <- lapply(who, function(x) of_landings(species = x))
names(by_species) <- who
dat <- melt(by_species, id = c("catch", "year"))[, -5]

## CB says: Looks like type is always "species", and you don't define metadata for it, so let's drop that column
dat <- dat[-3]
names(dat) <- c("catch", "year", "a3_code")
ggplot(dat, aes(year, catch)) + geom_line() + facet_wrap(~a3_code, scales = "free_y") + theme_bw()


library(data.table)
species <- data.table(species)
setkey(species, "a3_code")
code_names <- species[who, scientific_name]
codes <- code_names$scientific_name
names(codes) <- code_names$a3_code
codes

# plot the data
ggplot(dat, aes(year, catch)) + geom_line() + facet_wrap(~a3_code, scales = "free_y")

## year is a numeric, which needs an origin. Make it a character so R can interpret it as a date in %Y format
dat$year <- as.Date(as.character(dat$year), '%Y')


col.defs <- c(catch = "Global Landings of fish", 
              year = "the year for which data was reported", 
              a3_code = "3 digit country code")

unit.defs <- list("tonne", "YYYY", codes)

dats <- data.set(dat, col.defs=col.defs, unit.defs=unit.defs)

#install_github("EML", "cboettig", "devel")
require(EML)

## In EML-speak, description is means something different. The overall description of the EML file is the abstract (Think of this as a "data publication")  Yeah, so I royally broke the function API from your examples...
abstract <- "Landings data for several species by year, from the OpenFisheries database"



eml_write(dat = dats, title = "Landings Data", abstract = abstract, 
    creator = "Karthik Ram <karthik@ropensci.org>", file = "landings.xml")
 
 
eml_publish("landings.xml", description = abstract, categories = "Ecology", 
    tags = "fisheries", destination = "figshare", visibility = "public")
