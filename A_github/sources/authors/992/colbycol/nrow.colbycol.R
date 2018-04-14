#################################################################
# 
# File:         ncol.colbycol.r
# Purpose:      Gets the number of rows in colbycol object
#
# Created:      20090509
# Author:       Carlos J. Gil Bellosta
#
# Modifications: 
#
#################################################################

nrow.colbycol <- function( x )
{
    if( class( x ) != "colbycol" )
        stop("An object of class colbycol is required.")

    x$nrows
}
