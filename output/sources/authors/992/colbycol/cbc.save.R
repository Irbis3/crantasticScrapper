#################################################################
# 
# File:         cbc.save.R
# Purpose:      Saves a colbycol object in disk for later usage
#
# Created:      20110417
# Author:       Carlos J. Gil Bellosta
#
# Modifications: 
#
#################################################################

cbc.save <- function( data )
{
    if( class( data ) != "colbycol" )
        stop("An object of class colbycol is required.")

    cbc.metadata <- subset( data, names( data ) != "filehash" )

    data$filehash[[ "cbc.metadata" ]] <- cbc.metadata

    cat( paste( "Data has been stored in file ", data$filehash@datafile, "\n", sep = "" ) )
    cat( paste( 'Use cbc.load( "', data$filehash@datafile, '" ) to load it again\n', sep = "" ) )

    invisible( data$filehash@datafile  )
}
