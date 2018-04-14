#################################################################
# 
# File:         cbc.get.col.R
# Purpose:      Gets a single column from a colbycol object
#
# Created:      20090509
# Author:       Carlos J. Gil Bellosta
#
# Modifications: 
#               20110417: now data comes from a filehash structure
#
#################################################################

cbc.get.col <- function( data, column ) 
{
    if( class( data ) != "colbycol" )
        stop("An object of class colbycol is required.")

    if( missing( column ) )
        stop("A column name or number is required.")

    if( length( column ) != 1 )
        stop("Only a single column can be extracted at a time.")

    if( is.numeric( column ) ) {
        column <- colnames( data )[ column ]
        if( length( column ) != 1 )
            stop( "Index out of range." ) 
    }

    if( ! is.character( column ) )
        stop( "Value provided for column parameter cannot be recognized." )

    if( ! column %in% colnames( data ) )
        stop( "Column name cannot be found." )

    data$filehash[[ column ]]
}
