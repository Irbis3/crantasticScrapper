#################################################################
# 
# File:         summary.colbycol.r
# Purpose:      provides a short summary of a colbycol objetct
#
# Created:      20110319
# Author:       Carlos J. Gil Bellosta
#
# Modifications: 
#
#################################################################

summary.colbycol <- function( object, ... )
{
    if( class( object ) != "colbycol" )
        stop("An object of class colbycol is required.")

    cat( paste( "Object of class colbycol with ", nrow( object ), " rows and ", ncol( object ), " columns.\n", sep = "" ) )
    cat( paste( "Data for the object is stored at ", object$tmp.dir, ".\n", sep = "" ) )
}
