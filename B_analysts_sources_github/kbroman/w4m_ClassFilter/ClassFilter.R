#require(utils)
w4m_filter_imputation <- 
  function(m) {
    # replace NA values with zero
    m[is.na(m)] <- 0
    # replace negative values with zero, if applicable (It should never be applicable!)
    if (min(m < 0)) {
      m <- matrix(lapply(X = m, FUN = function(z) {max(z,0)}), nrow = nrow(m) )
    }
    # return matrix as the result
    return (m)
  }

#' Filter W4M Samples by Class of Sample
#'
#' Filter set of retention-corrected W4M files (dataMatrix, sampleMetadata) by feature
#'
#' @param dataMatrix_in        character: path to input file containing data matrix (tsv, rows are feature names, columns are sample names
#' @param sampleMetadata_in    character: path to input file containing sample metadata (tsv, rows are sample names, one column's name matches class_column)
#' @param variableMetadata_in  character: path to input file containing variable metadata (tsv, rows are variable names)
#' @param dataMatrix_out       character: path to output file containing data matrix (tsv, rows are feature names, columns are sample names
#' @param sampleMetadata_out   character: path to output file containing sample metadata (tsv, rows are sample names, one column's name matches class_column)
#' @param variableMetadata_out character: path to input file containing variable metadata (tsv, rows are variable names)
#' @param classes              character array: names of sample classes to include or exclude; default is an empty array
#' @param include              logical: TRUE, include named sample classes; FALSE (the default), exclude named sample classes
#' @param class_column         character: name of "class" column, defaults to "class"
#' @param samplename_column    character: name of column with sample name, defaults to "sampleMetadata"
#' @param data_imputation      function(m): default imputation method for 'intb' data, where intensities have background subtracted - impute zero for NA
#'
#' @return logical: TRUE only if filtration succeeded
#' 
#' @author Art Eschenlauer, \email{esch0041@@umn.edu}
## ' @references \code{\link{w4msampleclassfilter}}
#' @concept w4m workflow4metabolomics
#' @keywords multivariate
## ' @seealso \code{\link{https://github.com/HegemanLab/w4m_ClassFilter}}
## ' @seealso \code{\link{http://workflow4metabolomics.org/}}
#'
#' @export
w4m_filter_by_sample_class <- function(
  dataMatrix_in                           # character: path to input file containing data matrix (tsv, rows are feature names, columns are sample names)
, sampleMetadata_in                       # character: path to input file containing sample metadata (tsv, rows are sample names, one column is "class")
, variableMetadata_in                     # character: path to input file containing variable metadata (tsv, rows are variable names)
, dataMatrix_out                          # character: path to output file containing data matrix (tsv, rows are feature names, columns are sample names)
, sampleMetadata_out                      # character: path to output file containing sample metadata (tsv, rows are sample names, one column is "class")
, variableMetadata_out                    # character: path to output file containing variable metadata (tsv, rows are variable names)
, classes = c()                           # array of character: names of sample classes to include or exclude; default is an empty array
, include = FALSE                         # logical: TRUE, include named sample classes; FALSE (the default), exclude named sample classes
, class_column = "class"                  # character: name of "class" column, defaults to "class"
, samplename_column = "sampleMetadata"    # character: name of column with sample name, defaults to "sampleMetadata"
, data_imputation = w4m_filter_imputation # function(m): default imputation method is for 'intb' data, where intensities have background subtracted - impute zero for NA
) {
  # ---
  # define internal functions

  # MatVar - Compute variance of rows or columns of a matrix
  # ref: http://stackoverflow.com/a/25100036
  # For row variance, dim == 1, for col variance, dim == 2
  MatVar <- function(x, dim = 1) {
    if (dim == 1) {
      dim.x.2 <- dim(x)[2]
      if ( dim.x.2 == 0 )
        stop("MatVar: there are zero columns")
      if ( dim.x.2 == 1 ) {
        stop("MatVar: a single column is insufficient to calculate a variance")
        # return ( rep.int(x = 0, times = nrow(x)) )
      } else {
        return ( rowSums( (x    - rowMeans(x))^2 ) / ( dim(x)[2] - 1 ) )
      }
    } else if (dim == 2) {
      dim.x.1 <- dim(x)[1]
      if ( dim.x.1 == 0 ) {
        stop("MatVar: there are zero rows")
      }
      if ( dim.x.1 == 1 ) {
        stop("MatVar: a single row is insufficient to calculate a variance")
        # return ( rep.int(x = 0, times = ncol(x)) )
      } else {
        return ( rowSums( (t(x) - colMeans(x))^2 ) / ( dim(x)[1] - 1 ) )
      }
    } else stop("Please enter valid dimension, for rows, dim = 1; for colums, dim = 2")
  }

  # get names of columns that do not have only NA
  nonempty_column_names <-
    function(x) {
      # compute column sums; result is zero for columns having no non-NA values
      column_sum      <- sapply(1:ncol(x), function(i) sum(x[,i], na.rm = TRUE))
      
      # return names of columns 
      return ( as.character( colnames(x)[column_sum > 0.0] ) )
    }

  # produce matrix from matrix xpre where all rows and columns having zero variance have been removed 
  nonzero_var <- function(xpre) {
    nonzero_var_internal <- function(x) {
      if (nrow(x) == 0) {
          utils::str(x)
          stop("matrix has no rows")
      }
      if (ncol(x) == 0) {
          utils::str(x)
          stop("matrix has no columns")
      }
      if ( is.numeric(x) ) {
        # exclude any rows with zero variance
        row.vars <- MatVar(x, dim = 1)
        nonzero.row.vars <- row.vars > 0
        nonzero.rows <- row.vars[nonzero.row.vars]
        if ( length(rownames(x)) != length(rownames(nonzero.rows)) ) {
          row.names <- attr(nonzero.rows,"names")
          x <- x[ row.names, , drop = FALSE ]
        }

        # exclude any columns with zero variance
        column.vars <- MatVar(x, dim = 2)
        nonzero.column.vars <- column.vars > 0
        nonzero.columns <- column.vars[nonzero.column.vars]
        if ( length(colnames(x)) != length(colnames(nonzero.columns)) ) {
          column.names <- attr(nonzero.columns,"names")
          x <- x[ , column.names, drop = FALSE ]
        }
      }
      return (x)
    }

    # purge rows and columns that have zero variance until there are nor more changes
    #   rationale: nonzero_var_internal first purges rows with zero variance, then columns with zero variance,
    #              so there exists the possibility that a row's variance might change to zero when a column is removed;
    #              therefore, iterate till there are no more changes
    if ( is.numeric(xpre) ) {
      my.nrow <- 0
      my.ncol <- 0
      while ( nrow(xpre) != my.nrow || ncol(xpre) != my.ncol ) {
        my.nrow <- nrow(xpre)
        my.ncol <- ncol(xpre)
        xpre <- nonzero_var_internal(xpre)
      }
    }
    return (xpre)
  }
  # ...

  # return FALSE if any paths are exact duplicates
  my.paths <- c(dataMatrix_in, dataMatrix_out, sampleMetadata_in, sampleMetadata_out, variableMetadata_in, variableMetadata_out)
  if ( length(my.paths) != length(unique(my.paths)) ) {
    for ( my.path in my.paths ) {
      print(my.path)
    }
    stop("some paths are duplicated")
    return (FALSE)
  }

  # ---
  # read in the sample metadata
  err.env <- new.env()
  err.env$success <- FALSE
  err.env$msg <- "no message reading sample metadata"
  tryCatch(
    expr = {
      err.env$smpl_metadata <- utils::read.delim( fill = FALSE, file = sampleMetadata_in )
      err.env$success       <- TRUE
    }
  , error = function(e) {
     err.env$ msg <- "sampleMetadata_in read failed"
    }
  )
  if (!err.env$success) {
    print(err.env$msg)
    return ( FALSE )
  }
  smpl_metadata <- err.env$smpl_metadata

  # extract rownames
  rownames(smpl_metadata) <- smpl_metadata[,samplename_column]
  
  # select the first column of the rows indicated by classes, include, & class_column, but don't drop dimension
  selected_rows           <- smpl_metadata[ xor( !include, smpl_metadata[,class_column] %in% classes ), 1, drop = FALSE ]
  # obtain the row names
  sample_names            <- rownames( selected_rows )
  # ...

  # ---
  # read in the variable metadata
  err.env <- new.env()
  err.env$success <- FALSE
  err.env$msg <- "no message reading variable metadata"
  tryCatch(
    expr = {
      err.env$vrbl_metadata <- utils::read.delim( fill = FALSE, file = variableMetadata_in )
      err.env$success       <- TRUE
    }
  , error = function(e) {
     err.env$ msg <- "variableMetadata_in read failed"
    }
  )
  if (!err.env$success) {
    print(err.env$msg)
    return ( FALSE )
  }
  vrbl_metadata <- err.env$vrbl_metadata
  # extract rownames (using make.names to handle degenerate feature names)
  err.env <- new.env()
  err.env$success <- FALSE
  err.env$msg <- "no message setting vrbl_metadata rownames"
  tryCatch(
    expr = {
      rownames(vrbl_metadata) <- make.names( vrbl_metadata[,1], unique = TRUE )
      vrbl_metadata[,1] <- rownames(vrbl_metadata)
      err.env$success     <- TRUE
    }
  , error = function(e) {
     err.env$ msg <- sprintf("failed to set rownames for vrbl_metadata read because '%s'", e$message) 
    }
  )
  if (!err.env$success) {
    print(err.env$msg)
    return ( FALSE )
  }
  # ...

  # ---
  # read in the data matrix
  err.env <- new.env()
  err.env$success <- FALSE
  err.env$msg <- "no message reading data matrix"
  tryCatch(
    expr = {
      err.env$data_matrix <- utils::read.delim( fill = FALSE, file = dataMatrix_in )
      err.env$success     <- TRUE
    }
  , error = function(e) {
     err.env$ msg <- "dataMatrix_in read failed"
    }
  )
  if (!err.env$success) {
    print(err.env$msg)
    return ( FALSE )
  }
  data_matrix <- err.env$data_matrix

  # extract rownames (using make.names to handle degenerate feature names)
  err.env <- new.env()
  err.env$success <- FALSE
  err.env$msg <- "no message setting data_matrix rownames"
  tryCatch(
    expr = {
      rownames(data_matrix) <- make.names( data_matrix[,1], unique = TRUE )
      err.env$success     <- TRUE
    }
  , error = function(e) {
     err.env$ msg <- sprintf("failed to set rownames for data_matrix read because '%s'", e$message) 
    }
  )
  if (!err.env$success) {
    print(err.env$msg)
    return ( FALSE )
  }
  # ...

  # remove rownames column
  data_matrix <- data_matrix[,2:ncol(data_matrix)]

  # select the subset of samples indicated by classes, include, & class_column
  data_matrix <- data_matrix[,intersect(sample_names,colnames(data_matrix)), drop = FALSE]

  # impute missing values with supplied or default method
  data_matrix <- data_imputation(data_matrix)

  # purge data_matrix of rows and columns that have zero variance
  data_matrix <- nonzero_var(data_matrix)

  # purge smpl_metadata and vrbl_metadata of irrelevant rows
  sample_names <- intersect(sample_names,colnames(data_matrix))
  variable_names <- intersect( rownames(vrbl_metadata), rownames(data_matrix) )

  # ---
  # write out the results
  err.env <- new.env()
  err.env$success <- FALSE
  err.env$msg <- "no message writing output files"
  tryCatch(
    expr = {
      utils::write.table( x = data_matrix  [ rownames(data_matrix) %in% variable_names  , colnames(data_matrix) %in% sample_names, drop = FALSE ], file = dataMatrix_out      , sep = "\t", quote = FALSE, row.names = TRUE  , col.names = NA )
      utils::write.table( x = smpl_metadata[ sample_names                               ,                                        , drop = FALSE ], file = sampleMetadata_out  , sep = "\t", quote = FALSE, row.names = FALSE )
      utils::write.table( x = vrbl_metadata[ rownames(vrbl_metadata) %in% variable_names,                                        , drop = FALSE ], file = variableMetadata_out, sep = "\t", quote = FALSE, row.names = FALSE )

      err.env$success     <- TRUE
    }
  , error = function(e) {
     err.env$ msg <- sprintf("failed to set write output files because '%s'", e$message) 
    }
  )
  if (!err.env$success) {
    print(err.env$msg)
    return ( FALSE )
  }
  return (TRUE)
  # ...
}



  
