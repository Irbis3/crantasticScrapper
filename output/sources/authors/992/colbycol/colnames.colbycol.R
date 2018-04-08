#################################################################
# 
# File:         colnames.colbycol.r
# Purpose:      Print the colnames of a colbycol object
#
# Created:      20090509
# Author:       Carlos J. Gil Bellosta
#
# Modifications: 
#
#################################################################

colnames.colbycol <- function( x, do.NULL, prefix ) 
{
    if( class( x ) != "colbycol" )
        stop("An object of class colbycol is required.")

    names( x[["columns"]] )
}
