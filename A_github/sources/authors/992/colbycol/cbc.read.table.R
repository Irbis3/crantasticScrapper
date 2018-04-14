#################################################################
# 
# File:         cbc.read.table
# Purpose:      Read a large file and process it column by column
#
# Created:      20090506
# Author:       Carlos J. Gil Bellosta
#
# Modifications: 
#               20090729, CJGB: Changed underlying language from Python to Java
#               20110417, CJGB: Added support for filehash data storage
#
#################################################################

cbc.read.table <- function( file, filehash.name = tempfile( pattern = "filehash_" ), tmp.dir = tempfile( pattern = "dir" ), just.read = NULL, sample.pct = NULL, sep = "\t", header = TRUE, ... )
{

    # Checks parameters for errors

    if( ! file.exists( file ) )
        stop("Missing input file.")

    if( file.exists( file.path( getwd(), file ) ) )         # It was a relative path
        file <- file.path( getwd(), file )

    if( ! file.exists( tmp.dir ) && ! dir.create( tmp.dir, recursive = TRUE )  )
        stop("The destination directory cannot be either found or created.")

    if( ! dbCreate( filehash.name ) )
        stop( "Unable to create the filehash file" )

    my.filehash <- dbInit( filehash.name )

    # Sets original and target dirs

    original.dir <- getwd()
    on.exit( setwd( original.dir ) )

    setwd( tmp.dir )

    if( length( dir() > 0 ) )
        stop("The destination directory is not empty.")

    # Sips the data to get metadata info

    tmp.data <- read.table( file, nrows = 50, sep = sep, header = header, ... )

    columns <- sapply( 1:ncol( tmp.data ), function( i ) list( filename = formatC( i, width = 4, flag = "0" )), simplify = FALSE )
    names( columns ) <- colnames( tmp.data )

    skip <- ifelse( exists( "skip", mode = "numeric" ), skip, 0 ) + header

    # Check whether there are columns not to read

    cols.to.read <- 1:length( columns )

    if( ! is.null( just.read ) ){
        if( is.character( just.read ) )
            just.read <- which( just.read ) %in% names( columns )
        if( is.logical( just.read ) )
            just.read <- which( just.read )
        if( is.numeric( just.read ) )
            cols.to.read <- intersect( cols.to.read, just.read )
        if( ! is.numeric( just.read ) )
            stop( "The just.read argument needs to be either character, numeric or boolean" )
        if( length( just.read ) == 0 )
            stop( "There are no columns to read" )
    }

    # Setting up the sampling scheme

    if( !is.null( sample.pct ) ){
        if( ! is.numeric( sample.pct ) )
            stop( "Parameter sample.pct should either be NULL or a value in (0,1)")

        sample.pct <- max( 0, min( 1, sample.pct ) )

        if( sample.pct == 0 )
            stop( "Parameter sample.pct should be greater than 0" )

        if( sample.pct == 1 )
            sample.pct <- 10                # Java code does no sampling if sample.pct > 1
    }

    sample.pct <- ifelse( is.null( sample.pct ), 10, sample.pct )

    # Call to Java code via rJava
    .jcbc <- .jnew( "com/datanalytics/colbycol/ColByCol", 
                    file, 
                    paste( lapply( columns, function(x) x$filename ), collapse = ";" ),
                    as.integer( cols.to.read - 1 ),                 # Java starts counting from 0!
                    as.double( sample.pct ),
                    as.integer( skip ), sep )

    .jcall(.jcbc, "V", "execute", getwd() )        # Assigns the path in Jython; all required data resides there

    # TODO: check exceptions

    # At this point, the input files should already be sliced

    columns <- columns[ cols.to.read ]

    if( file.info( columns[[1]]$filename )$size == 0 )
        stop( "No rows to read" )

    for( column in names(columns) ){
        tmp <- read.table( columns[[column]]$filename, sep = sep, na.strings = "", comment.char = "", quote = "", header = FALSE, ... )[,1]
        columns[[column]] <- c( columns[[column]], list( class = class( tmp ) ) )
        my.filehash[[column]] <- tmp 
        nrows <- length( tmp )
        rm( tmp )
        gc()
    }

    tmp <- list( filename = file, tmp.dir = tmp.dir, columns = columns, nrows = nrows, filehash = my.filehash )
    class( tmp ) <- "colbycol"
    tmp

}
