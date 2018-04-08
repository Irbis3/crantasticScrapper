#################################################################
# 
# File:         cbc.load.R
# Purpose:      Loads a saved colbycol object for further processing
#
# Created:      20110417
# Author:       Carlos J. Gil Bellosta
#
# Modifications: 
#
#################################################################

cbc.load <- function( file.name )
{
    if( ! file.exists( file.name ) )
        stop( "File cannot be found" )

    my.filehash <- dbInit( file.name )              # fails if filename does not point to filehash structure

    cbc.metadata <- my.filehash[[ "cbc.metadata" ]]
    cbc.metadata$filehash <- my.filehash

    class( cbc.metadata ) <- "colbycol" 

    cbc.metadata
}
