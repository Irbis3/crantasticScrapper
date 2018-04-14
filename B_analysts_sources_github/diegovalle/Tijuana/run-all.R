########################################################
#####       Author: Diego Valle Jones
#####       Website: www.diegovalle.net
#####       Date Created: Fri Jun  3 21:37:00 2011
########################################################
#Charts of Homicides in Baja California

#Helper Functions
source("src/load-libraries.R")
source("src/utilities.R")
source("src/summary-stats-functions.R")
source("src/label-charts.R")
theme_set(theme_bw())

#Make the charts
source("src/rates.R")
source("src/load-data.R")
source("src/charts.R")

#Homicides that occurred in 2008 but were registered in 2009
nrow(subset(hom.tj, ANIODEF == 2008 & ANIOREG == 2008))
nrow(subset(hom.tj, ANIODEF == 2008 & ANIODEF == 2008))

ddply(hom.mexic, .(ANIODEF), nrow)

ddply(deaths, .(ANIODEF, NECROPCIAtxt, PRESUNTOtxt), nrow)


