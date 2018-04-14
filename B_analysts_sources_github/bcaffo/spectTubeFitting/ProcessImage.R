#############################################################
# Aug 29 2011
# Jeff Goldsmith and Brian Caffo
#
# this file is for end users for constructing concentration-
# by-distance curves. it asks the user to locate the primary
# helper file, the image file, and to set some tuning 
# parameters as necessary. it then opens an interactive 
# graphic to set endpoints, fits a centerline, removes points
# outside an set
#############################################################

rm(list = ls())

library(stats)
library(graphics)
library(AnalyzeFMRI)
library(splines)
library(rgl)
library(scatterplot3d)


#############################################################
# find "DoFitting.R"
#############################################################

## Find "DoFitting.R" on your machine.
helper.file = file.choose()


#############################################################
# find the image file and set tuning parameters. this step 
# needs to be repeated for each image.
#############################################################

## Choose the image file you wish to process.
image.file = file.choose()


## Set tuning Parameters
PARABOLA = FALSE     # TRUE fits a parabola; FALSE fits a more
                     # flexible curve

maxDf <- 4           # controls flexibility of curve if PARABOLA
                     # is FALSE

dist.threshold = 5   # max distance (in cm) that points included in
                     # the concen-by-dist curve can be from the 
                     # centerline


#############################################################
# execute the program
#############################################################

source(helper.file)



#############################################################
#############################################################
#############################################################
#############################################################
#############################################################
#############################################################