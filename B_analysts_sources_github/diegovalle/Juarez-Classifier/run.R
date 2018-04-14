########################################################
#####       Author: Diego Valle Jones
#####       Website: www.diegovalle.net
########################################################
#This program classifies deaths of unknown intent in
#Ciudad Juárez


#The classifiers will take a long time to run

source(file.path("src", "load-packages.R"))
##Read the subset of the mortality database corresponding to Juárez
hom.juarez <- read.csv("data/juarez.csv.bz2")

##Add the external cause of injury mechanism to the database
source(file.path("src", "codeMM.R"))
test_dir("tests")
#Plot missing data
##Replace 998 with NA, etc
source(file.path("src", "clean-data.R"))
##Unit testing
source(file.path("src", "plot-missing.R"))
#Use machine learning to classify deaths of unknown intent
source(file.path("src", "classifier.R"))
#Plot the imputed homicides
source(file.path("src", "plot-imputed.R"))

