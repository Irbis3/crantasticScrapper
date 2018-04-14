#################################################################
# 
# File:         ncol.colbycol.r
# Purpose:      Gets the number of columns in a colbycol object
#
# Created:      20090509
# Author:       Carlos J. Gil Bellosta
#
# Modifications: 
#
#################################################################

ncol.colbycol <- function( x ) 
{
    if( class( x ) != "colbycol" )
        stop("An object of class colbycol is required.")

    length( colnames( x ) )
}
