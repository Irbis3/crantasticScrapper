#' Safe subset
#'
#' @param df Dataframe
#' @param column One name of column to subset within
#' @param subset Vector of entries in column to subset to
#'
#' If column not in df, returns back the df
safeSubset <- function(df, column, subset){

  testthat::expect_is(df, "data.frame")
  testthat::expect_is(column, "character")
  testthat::expect_equal(length(column), 1)

  if(!is.null(subset)){
    testthat::expect_is(subset, "character")
  } else {
    message("Subset is NULL, returning original")
    out <- df
  }

  message(" # subsetting # original rows: ",nrow(df) ," column:", column, " by ", paste(subset, collapse = ", "))

  col <- df[[column]]

  if(!is.null(col)){
    out <- df[col %in% subset,]
    message("Subset rows: ", nrow(out))
  } else {
    message("Column not found:", column)
    out <- df
  }

  out

}


#' str all the things in environment
#'
#'
strAll <- function(){
  lapply(ls(), function(x) str(get(x)) )
}

#' Start of the month
#' @param x A date
som <- function(x) {
  as.Date(format(x, "%Y-%m-01"))
}

#' End of the month
#' @param x A date
eom <- function(x) {
  som(som(x) + 35) - 1
}

#' Start and end of last month
get_start_end_month <- function(x = Sys.Date()){
  c(som(som(x) - 1), som(x) - 1)
}


#' Gets the names of a dataframe's columns of a certain class
#'
#' @param df dataframe
#' @param class Col type to return e.g. "character"
#'
getColNameOfClass <- function(df, class_name){
  stopifnot(inherits(df, "data.frame"),
            inherits(class_name, "character"))

  names(df)[vapply(df, class, "string") == class_name]
}

#' check a Shiny select input
#'
#' Covers multiple and single select
checkSelectInput <- function(input_select){
  if(all(!is.null(input_select), input_select != "")){
    TRUE
  } else {
    FALSE
  }
}


#' Change data frame factor colums to character
#'
#' @param dframe a data.frame with factor colums
#'
#' @return dframe with factor columns turned into character
changeDFFactorsToCharacter <- function(dframe){
  stopifnot(inherits(dframe,"data.frame"))

  factors <- sapply(dframe, is.factor)
  dframe[factors] <- lapply(dframe[factors], as.character)
  dframe
}




#' Write an object to csv
#'
#' @param df A dataframe
#' @param folder Where to write it
#'
#' Will write csv file with name folder+df_name+date+.csv
writeFile <- function(df, folder = "./data/"){
  df_name <- deparse(substitute(df))

  file_name <- paste0(folder,df_name,"_",Sys.Date(),".csv")
  write.csv(df, file = file_name, row.names = FALSE)

}



#' Perform lookup
#'
#' Modifies input by a named vector where names = original, values = lookup
#'
#' @param input character vector to modify
#' @param lookup Named character vector
#' @param no_match If no_match=TRUE then no matches become "Other"
#'
#' Modifies input in place, leaves vector not in lookup as is
lookupNames <- function(input, lookup, no_match = FALSE){
  stopifnot(inherits(input,"character"), inherits(lookup, "character"))

  if(no_match){
    input <- lookup[input]
    input[is.na(input)] <- "Other"
  } else {
    input[input %in% names(lookup)] <- lookup[input[input %in% names(lookup)]]
  }

  output <- input
}


#' Customer message log level
#'
#' @param ... The message(s)
#' @param level The severity
#'
#' @details 0 = everything, 1 = debug, 2=normal, 3=important
myMessage <- function(..., level = 1){


  compare_level <- getOption("googleAuthR.verbose")
  if(is.null(compare_level)) compare_level <- 1

  if(level >= compare_level){
    message(Sys.time()," -- ", ...)
  }
  
  if(level == 0){
    cat(file = stderr(), ...)
  }
  
}



#' Idempotency
#'
#' A random code to ensure no repeats
#'
#' @return A random 15 digit hash
#'
#' @export
idempotency <- function(){
  paste(sample(c(LETTERS, letters, 0:9), 15, TRUE),collapse="")
}


#' Converts RFC3339 to as.Date
#'
#' @keywords internal
RFC_convert <- function(RFC, drop_time=FALSE){

  if(drop_time){
    d <-   as.Date(strptime(as.character(RFC),
                            tz="UTC",
                            "%Y-%m-%dT%H:%M:%OSZ"))
  } else {
    d <- strptime(as.character(RFC),
                  tz="UTC",
                  "%Y-%m-%dT%H:%M:%OSZ")
  }

  return(d)
}

#' Is this a try error?
#'
#' Utility to test errors
#'
#' @param test_me an object created with try()
#'
#' @return Boolean
#'
#' @keywords internal
is.error <- function(test_me){
  inherits(test_me, "try-error")
}

#' Get the error message
#'
#' @param test_me an object that has failed is.error
#'
#' @return The error message
#'
#' @keywords internal
error.message <- function(test_me){
  if(is.error(test_me)) attr(test_me, "condition")$message
}


#' A helper function that tests whether an object is either NULL _or_
#' a list of NULLs
#'
#' @keywords internal
is.NullOb <- function(x) is.null(x) | all(sapply(x, is.null))

#' Recursively step down into list, removing all such objects
#'
#' @keywords internal
rmNullObs <- function(x) {
  x <- Filter(Negate(is.NullOb), x)
  lapply(x, function(x) if (is.list(x)) rmNullObs(x) else x)
}

#' Camel case to dot.case
#'
#' @param character vector
#'
#' @return All camelCase becomes lowercase with dots e.g. camel.case
camelToDot <- function(camelCase){
  s <- gsub("([a-z])([A-Z])", "\\1.\\L\\2", camelCase, perl = TRUE)
  sub("^(.[a-z])", "\\L\\1", s, perl = TRUE) # make 1st char lower case
}

#' camelCase to Title Case
#'
#' @param character vector
#'
#' @return All camelCase becomes Title with spaces e.g. Camel Case
camelToTitle <- function(camelCase){
  s <- camelToDot(camelCase)
  s <- gsub("."," ",s, fixed=TRUE)

  stringr::str_to_title(s)
}

#' Pretty display names
#'
#' @param choice vecotr to get different names
#' @param displayNames names to replace choice
#'
#' @return choice named with pretty names
#'
getDisplayNames <- function(choice, displayNames){
  if(!is.null(displayNames)){
    overwrite <- displayNames[choice]
    overwrite[is.na(overwrite)] <- choice[is.na(overwrite)]
    names(choice) <- overwrite
  }

  choice

}

## modify the defaults if ... has been used to specify
default_overwrite <- function(..., overwrite){

  default_args <- list(...)

  if(inherits(overwrite, "list")){
    out <- modifyList(default_args, overwrite, keep.null = TRUE)
  } else {
    out <- default_args
  }

  out

}

pad_digits <- function(n, pad=2){
  gsub(" ","0", sprintf(paste0("%",pad,"d"), n))
}

diff_pages <- function(data_frame, column_name, suffix){

  last <- data_frame[,paste0(column_name,suffix[1])]
  before <- data_frame[,paste0(column_name,suffix[2])]

  last[is.na(last)] <- 0
  before[is.na(before)] <- 0

  out <- last - before

  out

}

diff_dataframe <- function(data_frame, col_names, suffix = c(".lastmonth",".monthbefore"), outsuffix=".diffmonth"){

  out <- as.data.frame(lapply(col_names, function(x) diff_pages(data_frame, x, suffix)))
  names(out) <- paste0(col_names, outsuffix)

  out
}



#' Add name of list entry of dataframe to dataframe colum
#'
listNameToDFCol <- function(named_list, colName = "listName"){
  lapply(names(named_list),
         function(x) {named_list[[x]][colName] <- x
         named_list[[x]]
         })
}

#' Make start and end month date range
#'
#' @param the_date A date to get previous month end and start
#'
#' @return list of $start and $end
#'
#' @import lubridate
#' @export
month_start_end <- function(the_date = Sys.Date()){

  if(is.null(the_date)) the_date <-  Sys.Date()

  the_data <- as.Date(the_date)

  start <- floor_date(the_date %m-% months(1), unit = "month")
  end <- ceiling_date(the_date %m-% months(1), unit = "month")

  ## a bug if start of the month?
  if(start == end){
    end <- ceiling_date((the_date + 1) %m-% months(1), unit = "month")
  }

  end <- end - 1

  list(start = start, end = end)

}
