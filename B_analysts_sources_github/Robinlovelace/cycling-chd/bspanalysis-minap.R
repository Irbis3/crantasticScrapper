###########################################
######## Bayesian Spatial Analysis ########
###########################################


# Libraries
library(data.table)
library(plyr)
# source("http://www.math.ntnu.no/inla/givemeINLA.R") # To install
library(INLA)

### Sort out data ###

# Load data
minap_msoas <- readRDS("data/msoas_observed_expected_counts.Rds")

# Aggregate by MSOA
dt <- data.table(minap_msoas)
msoa_persons <- dt[, list(admissions = sum(admissions, na.rm = TRUE), expt_adms = sum(expt_adms, na.rm = TRUE)),
                   by = c("msoa_code")]
msoa_sex <- dt[, list(admissions = sum(admissions, na.rm = TRUE), expt_adms = sum(expt_adms, na.rm = TRUE)),
               by = c("sex", "msoa_code")]
msoa_males <- msoa_sex[msoa_sex$sex=="Male"]
msoa_females <- msoa_sex[msoa_sex$sex=="Female"]
rm(minap_msoas)
rm(msoa_sex)
rm(dt)
gc()


## Exposure variables ##

# Active Transport #

# Load transport data for MSOAs
msoa_transport <- readRDS("data/msoas_transport_data.Rds") # Load
msoa_transport$msoa_code <- msoa_transport$geo_code
msoa_transport$geo_code <- NULL

# Calculate exposure variables
msoa_transport$pc_cycle <- (msoa_transport$Bicycle / msoa_transport$All) * 100 # cycle
msoa_transport$pc_walk <- (msoa_transport$foot / msoa_transport$All) * 100 # walk
msoa_transport$pc_active <- msoa_transport$pc_cycle + msoa_transport$pc_walk # Active transport
msoa_transport$pc_car <- (msoa_transport$Car / msoa_transport$All) * 100 # car
msoa_transport <- msoa_transport[,5:9] # drop variables not needed

# Join on cycling data
msoa_p <- join(msoa_persons, msoa_transport, by = c("msoa_code"), type = "left", match = "all")
msoa_m <- join(msoa_males, msoa_transport, by = c("msoa_code"), type = "left", match = "all")
msoa_f <- join(msoa_females, msoa_transport, by = c("msoa_code"), type = "left", match = "all")
rm(msoa_transport)
rm(msoa_persons)
rm(msoa_females)
rm(msoa_males)


# Deprivation #

# Load IMD15 MSOA estimates
msoa_imd <- read.csv("data/imd15.csv")

# Join on IMD15 MSOA estimates
msoa_p <- join(msoa_p, msoa_imd, by = c("msoa_code"), type = "left", match = "all")
msoa_m <- join(msoa_m, msoa_imd, by = c("msoa_code"), type = "left", match = "all")
msoa_f <- join(msoa_f, msoa_imd, by = c("msoa_code"), type = "left", match = "all")
rm(msoa_imd)

# Drop missing data (i.e. only england MSOAs - n=6147)
eng_p <- na.omit(msoa_p)
eng_m <- na.omit(msoa_m)
eng_f <- na.omit(msoa_f)
rm(msoa_p)
rm(msoa_f)
rm(msoa_m)


# Create shapefile for INLA to work with
# library(maptools)
# library(spdep)
# england <- readShapePoly("./GIS/England_msoa_2011/england_msoa_2011.shp") # Load data (will take a few mins)
# temp <- poly2nb(england) # identify neighbours of MSOAs (will take a few mins)
# nb2INLA("./GIS/England_msoa_2011/england.graph", temp) # convert and save in INLA format
# ldn.adj <- paste(getwd(),"/GIS/England_msoa_2011/england.graph", sep="")
H <- inla.read.graph("./GIS/England_msoa_2011/england.graph") # Load neighbour adjacency matrix into R
# image(inla.graph2matrix(H), xlab="", ylab="") # Visualise adjacency matrix
eng_p$n <- 1:nrow(eng_p) # So the unique ID is same as adjacency matrix (which just refers to rownumber)
eng_m$n <- 1:nrow(eng_m)
eng_f$n <- 1:nrow(eng_f)


##### Analysis #####

# Persons #

# Poisson regression
formula <- admissions ~ 1 + pc_cycle + pc_walk + imd15_score + f(n, model = "bym", graph = H)
pmodel_p <- inla(formula, family = "poisson", data = eng_p, offset = log(expt_adms), control.compute=list(dic=T)) # Wil take 5 mins or so to run
summary(pmodel_p)

# Negative binomial model
nbmodel_p <- inla(formula, family = "nbinomial", data = eng_p, offset = log(expt_adms), control.compute=list(dic=T)) # Wil take 5 mins or so to run
summary(nbmodel_p)

exp(nbmodel_p$summary.fixed) # Incidence Rate Ratios

# exp.fixed.maginal = inla.tmarginal(function(x) exp(x), nbmodel_p$marginals.fixed$pc_walk) # If you want to plot the density of the marginals
# plot(exp.fixed.maginal)

# # Zero inflated model
# zpmodel_p <- inla(formula, family = "zeroinflatedpoisson1", data = eng_p, E = expt_adms, control.compute=list(dic=T))
# summary(zpmodel_p)


# By Sex #

# Negative binomial model
nbmodel_f <- inla(formula, family = "nbinomial", data = eng_f, offset = log(expt_adms), control.compute=list(dic=T)) # Wil take 5 mins or so to run
nbmodel_m <- inla(formula, family = "nbinomial", data = eng_m, offset = log(expt_adms), control.compute=list(dic=T)) # Wil take 5 mins or so to run
summary(nbmodel_f)
summary(nbmodel_m)
exp(nbmodel_f$summary.fixed) # Incidence Rate Ratios
exp(nbmodel_m$summary.fixed) # Incidence Rate Ratios

