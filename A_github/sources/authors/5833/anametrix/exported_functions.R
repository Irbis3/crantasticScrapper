#================================================#
#
# Title:		Exported functions
#
#================================================#

# Only exported functions are accessible when the package is loaded.
# However inside those exported function, hidden_functions.R functions can be used.

#================================================#
# list of exported functions --

#--# Authentication functions
# authenticate
# authenticationCheck    

#--# Download/upload functions
# getTableConfiguration
# printTable
# getAnametrixDataset
# getAxDatasetFromEncodedQuery
# uploadData

#--# Metadata functions
# getAccountInfo
# getDatasuiteInfo
# getSegmentInfo
# getTableInfo
# getVariableInfo
# getDetailedInfo
# getReplacementNames


#------------------------------------------------#
#      Authentication functions----                
#------------------------------------------------#

#================================================#
#' authenticate
#'
#' @description Authenticates to the Anametrix platform. 
#' 
#' @details return a list of authentication elements (API URL, access token) used to download and upload data to the platform. 
#'
#' @param axapi.uri default is "https://ui.anametrix.com/api". URI is different for R Jobs.
#' @param username most often an anametrix email (e.g. "demo.user@demoanametrix.com")
#' @param password associated password
#' 
#' @examples 
#' username <- "demo.user@demoanametrix.com"
#' password <- "password"
#' auth     <- authenticate("https://demo.anametrix.com/api", username, password)
#'
#' @family authentication functions
#' 
#' @export
#' 
authenticate <- function(axapi.uri = "https://ui.anametrix.com/api", username, password) {
  if (as.character(axapi.uri) == "") {
    cat("You need to specify API Endpoint URL correctly. API URL Invalid", "\n")
    return(NULL)
  }
  
  auth.XML.req	<- paste(sep = "",
                        "cmd=<getAuthenticationToken xmlns=\"http://api.anametrix.com/api-envelope.xsd\">",
                        "<username>", username, "</username>\n   \t<password>",
                        password, "</password>\n <apikey>",
                        "R-AX-MODULE-API-KEY", "</apikey>\n <client>",
                        "R", "</client></getAuthenticationToken>")
  
  tryCatch(resp <- getURL(axapi.uri, ssl.verifypeer = FALSE, postfields = auth.XML.req,
                          .opts = list(timeout = 100), useragent = "R-Authentication"),
           error = function(err) {
             cat("Failed to authenticate with Anametrix API", "\n")
             cat("Error:", conditionMessage(err), "\n")
             return(NULL)
           })
  
  doc <- xmlTreeParse(resp)
  root <- xmlRoot(doc)
  auth.token <- xmlAttrs(root)["data"]
  
  if (is.na(auth.token)) {
    cat("Failed to authenticate with Anametrix API", "\n",
        "Make sure credentials are specified correctly", "\n")
    return(NULL)
  }
  
  cat("\nConnected ...")
  object			<- list(axapiUri = axapi.uri, username = username, password = password, authtoken = auth.token)
  class(object) 	<- "authentication"
  
  cat(" Authenticated.\n\n")
  
  return(object)
}

#================================================#
#' authenticationCheck
#'
#' @description checking if well authenticated
#' 
#' @details Checks if an existing authentication instance is still valid to access the platform.
#'
#' @param auth Authentication instance. result of \code{\link{authenticate}} function
#' 
#' @return boolean.
#' 
#' @examples 
#' username <- "demo.user@demoanametrix.com"
#' password <- "password"
#' auth     <- authenticate("https://demo.anametrix.com/api", username, password)
#' authenticationCheck(auth)
#'
#' @family authentication functions
#'
#' @export
#' 

authenticationCheck <- function(auth) {
  if (!exists(as.character(substitute(auth)), envir = .GlobalEnv)){
    cat("Authentication missing ... get new authentication credentials:  \n\n")
    cat("	auth <- authenticate(axapi.uri, username, password)", "\n\n")
    return(FALSE) 
  }
  if (is.null(auth) || is.null(auth$authtoken) || !isAuthTokenValid(auth)) {
    cat("Authentication missing ... get new authentication credentials:  \n\n")
    cat("	auth <- authenticate(axapi.uri, username, password)", "\n\n")
    return(FALSE)
  } else
    return(TRUE)
}



#------------------------------------------------#
#       Download/upload  functions----
#------------------------------------------------#

#================================================#
#' getTableConfiguration
#'
#' @description Get configuration information about table table.name from datasuite report.suite.id
#'
#' @param auth Authentication instance. result of \code{\link{authenticate}} function
#' @param report.suite.id ID number (as numeric or character) of the data suite
#' @param table.name name of table found in 'name' field of table information, i.e. not the tableName field
#' @param table.id should only be defined if table.name is not (otherwise, only table.id will be kept). Useful if two tables have the same name.
#'
#' @return List of objects defining the table configuration, used to download data from the Anametrix platform.
#' 
#' @examples
#' username <- "demo.user@demoanametrix.com"
#' password <- "password"
#' if (!authenticationCheck(auth)) 
#' auth <- authenticate("https://demo.anametrix.com/api", username, password)
#' browser.table    <- getTableConfiguration(auth, 10385, "Browsers")
#' 
#' @family dowload/upload functions
#' 
#' @export
#' 
getTableConfiguration <- function(auth, report.suite.id, table.name = NULL, table.id = NULL) {
  if (!authenticationCheck(auth)) return(NULL)
  if (!apiUrlCheck(auth)) return(NULL)
  
  full.table.list.XML  <- getFullTableListXML(auth, report.suite.id)
  table.list.XML  	   <- tryCatch(full.table.list.XML[[1]], error = function(e) NULL)
  if (is.null(table.list.XML)) {
    print("access denied or no table accessible")
    return(NULL)
  }
  
  if (is.null(table.id)){
    if (is.null(table.name)) {
      print("Table name and Table ID missing. Please define one of them.")
      return(NULL)
      break
    } else {
      
      for (i in 1:xmlSize(table.list.XML)) {
        tableXML    	<- table.list.XML[[i]]
        name				<- xmlAttrs(tableXML)["name"]
        
        if (as.character(name) == table.name) {
          
          #register all column names
          AllColumns			<- vector(length=xmlSize(tableXML[[1]]))
          for (i in 1:xmlSize(tableXML[[1]])) {
            AllColumns[i]		<- xmlAttrs(tableXML[[1]][[i]])["columnName"]
          }
          
          #register all column labels
          AllLabels			<- vector(length=xmlSize(tableXML[[1]]))
          for (i in 1:xmlSize(tableXML[[1]])) {
            AllLabels[i]		<- xmlAttrs(tableXML[[1]][[i]])["name"]
          }
          
          object				<- list(auth = auth, report.suite.id = report.suite.id, table.name = table.name,
                            tableXML = tableXML, ColumnNames = AllColumns ,ColumnLabels = AllLabels)
          class(object)		<- "table.object"
          return(object)
          break
        }
      }
    }
    
  } else {
    if (!is.null(table.name)) print("table.name not taken into account since table.id is defined.")
    
    for (i in 1:xmlSize(table.list.XML)) {
      tableXML    	<- table.list.XML[[i]]
      id				<- xmlAttrs(tableXML)["id"]
      
      if (as.character(id) == as.character(table.id)) {
        
        #register all column names
        AllColumns			<- vector(length=xmlSize(tableXML[[1]]))
        for (i in 1:xmlSize(tableXML[[1]])) {
          AllColumns[i]		<- xmlAttrs(tableXML[[1]][[i]])["columnName"]
        }
        
        #register all column labels
        AllLabels			<- vector(length=xmlSize(tableXML[[1]]))
        for (i in 1:xmlSize(tableXML[[1]])) {
          AllLabels[i]		<- xmlAttrs(tableXML[[1]][[i]])["name"]
        }
        
        object				<- list(auth = auth, report.suite.id = report.suite.id, table.name = table.name,
                          tableXML = tableXML, ColumnNames = AllColumns ,ColumnLabels = AllLabels)
        class(object)		<- "table.object"
        return(object)
        break
      }
    }
  }
  
  rm(i)
  print(paste("Table", table.name, "could not be found.", sep = " "))
  return(NULL)
}

#================================================#
#' printTable
#' 
#' @description Prints configuration of a table object extracted from Anametrix API
#'
#' @param table.object Configuration of a table. Use  \code{\link{getTableConfiguration}} to get it
#' 
#' @export
printTable <- function(table.object) {
  cat(paste(sep = " ", "Table:", xmlAttrs(table.object$tableXML)["name"], ", Data source:",
            xmlAttrs(table.object$tableXML)["datasource"], "\n"))
  
  columnListXML			<- table.object$tableXML[[1]]
  for (i in 1:xmlSize(columnListXML)) {
    columnXML				<- columnListXML[[i]]
    name					<- xmlAttrs(columnXML)["name"]
    columnName				<- xmlAttrs(columnXML)["columnName"]
    cat(paste(sep = " ", "Column:", name, ", internal name:", columnName, "\n"))
  }
  
  rm(i, name, columnName)
}


#================================================#
#' getAnametrixDataset
#'
#' @description Extracts data from Anametrix API into an R data frame
#'
#' @param table.object Configuration of a table. Use  \code{\link{getTableConfiguration}} to get it
#' @param segment.id segment id to apply (see \code{\link{getSegmentInfo}}). NULL is no segment is wanted. 
#' @param columns Array of column names for the request. Example: c("city","country","male_population")
#'                Use \code{printTable(tableObject)} to see all column names (internal names are used) or the field $ColumnNames of the output of \code{\link{getTableConfiguration}}
#'                If columns is an empty vector, all the columns of the table will be downloaded.
#' @param filter.def equal or in this list filter. Needs to be of the form \code{filter.def = list(column=NULL, type=c("equals", "in this list"), value=NULL)}
#' @param start.date Start date of the request. Use YYYY-MM-DD format. Example: "2015-01-01"
#' @param end.date End date of the request. Use YYYY-MM-DD format. Example: "2016-01-01"
#' @param max.rows Number of rows requested. Example: 10000
#' @param unit.of.time Format of the date column. Example: "day", "hour", "week", or custom format like "MM-dd-yy" or "w-yyyy"
#' @param sort.column Column to sort on. Use any column provided in the columns array
#' @param sort.direction "desc" or "asc"
#' @param verbose Specifies whether to print out notifications
#' @param date.format.override TRUE or FALSE. Adjusts date formatting
#' @param return.as.date.selection NULL, TRUE or FALSE. Adjusts date formatting
#' @param use.binary.func TRUE or FALSE
#'
#' @details 
#' if \code{use.binary.func} is FALSE, getURL is used to download text to a temporary file that is read in a dataframe
#' if \code{use.binary.func} is TRUE, getBinaryURL is used to download binary data. This can be used to avoid error in encoding like embedded null. No temporary file is used.
#'
#' @return R data frame containing the number of observations requested from the Anametrix API table
#'
#' 
#' @family dowload/upload functions
#
#' @export
getAnametrixDataset <- function(table.object, segment.id = NULL, columns=NULL,
                                filter.def = list(column=NULL, type=c("equals", "in this list"), value=NULL),
                                start.date, end.date, max.rows = 10E7, unit.of.time = "day", sort.column=NULL, sort.direction="asc", verbose=TRUE,
                                date.format.override=FALSE, return.as.date.selection=FALSE, use.binary.func = TRUE) {
  
  # return.as.date.selection possible values : NULL TRUE FALSE
  # TRUE TRUE or TRUE NULL will give a full date as character (ex : Sun Jun 28 00:00:00 PDT 2015)
  # FALSE FALSE will give "2015-06-29"
  
  # use.binary.func
  # if FALSE, getURL downloads text to a temporary file that is read in a dataframe
  # if TRUE, getBinaryURL is used to download binary data. This can be used to avoid error in encoding like embedded null. No temporary file is used.
  
  auth <- table.object$auth
  report.suite.id <- table.object$report.suite.id
  
  if (!authenticationCheck(auth)) return(NULL)
  if (!apiUrlCheck(auth)) return(NULL)
  
  if (is.null(columns))
    columns				<- table.object$ColumnNames
  if (is.numeric(columns))
    columns				<- table.object$ColumnNames[columns]
  if (is.null(sort.column))
    sort.column			<- columns[1]
  
  max.rows <- formatC(max.rows, format = "f", digits = 0)
  query.XML <- constructQueryXML(auth, report.suite.id, table.object, segment.id, columns, filter.def,
                                 start.date, end.date, max.rows, unit.of.time, sort.column, sort.direction,
                                 date.format.override, return.as.date.selection)
  query.XML.req <- paste("cmd=", curlEscape(toString.XMLNode(query.XML)))
  h <- getCurlHandle()
  reader <- queryReader(columns, verbose)
  
  cat("Validating URL ...")
  
  #--# download text if use.binary.func is FALSE
  if (!use.binary.func) {
    
    tryCatch(getURL(auth$axapiUri, ssl.verifypeer = FALSE, followLocation = TRUE, postfields = query.XML.req,
                    encoding = "UTF-8", curl = h, .opts = list(timeout = 3600, verbose = T), useragent = "R",
                    write = chunkToLineReader(reader$read)$read, verbose = F)
             , error = function(err) {
               cat("\nError occurred when retrieving data:", conditionMessage(err), "\n")
               #close(reader$fileCon())
               #file.remove(reader$filename())
               return(NULL)
             }
    )
    #--# download binary data if use.binary.func is TRUE
  } else {
    dataRaw <- tryCatch(getBinaryURL(auth$axapiUri, ssl.verifypeer = FALSE, followLocation = TRUE, postfields = query.XML.req,
                                     encoding = "UTF-8", curl = h, .opts = list(timeout = 3600, verbose = FALSE), useragent = "R",
                                     verbose = FALSE)
                        , error = function(err) {
                          cat("\nError occurred when retrieving data:", conditionMessage(err), "\n")
                          return(NULL)
                        }
    )
    
    dataNul <- dataRaw == as.raw(0)       # find where the NULLs are if any
    dataRaw[dataNul] <- as.raw(20)        # modify the new vector NULLs to SPACEs
    dataChar <- rawToChar(dataRaw)        # you can now convert these to Char
    
    tryCatch(reader$read(dataChar), error = function(err) {
      cat("\nError occurred when retrieving data:", conditionMessage(err), "\n")
      return(NULL)
    })
  }
  cat(" validated.\n")
  
  if (verbose)
    cat("Anametrix server responding ...")
  close(reader$fileCon())
  
  if (file.info(reader$filename())$size > 0) {
    DF <- read.table(reader$filename(), sep = "\t", header = T, numerals = "no.loss", quote = "\"", comment.char = "")
    if ((nrow(DF) + 1) != length(readLines(reader$filename()))) { # check if all the file is read properly, is case of nested quotes for example
      DF_names <- names(DF)
      DF <- read.table(reader$filename(), sep = "\t", header = T, numerals = "no.loss", quote = "", comment.char = "") %>%
        sapply(function(x) gsub("\"$", "", gsub("^\"", "", x))) %>% as.data.frame()
      for (col in names(DF))
        DF[, col] <- type.convert(DF[ , col])
      names(DF) <- DF_names
    }
    cat(" done.\n")
  } else {
    cat("\nError occurred when retrieving data:  \n",
        "   Make sure data suite, table, columns, segment, and filter are specified correctly, \n    otherwise table may be empty or the query simply 'timed out.'\n")
    file.remove(reader$filename())
    return(NULL)
  }
  
  file.remove(reader$filename())
  rm(reader)
  
  # if (!is.null(save.query.XML) && is.character(save.query.XML)) {
  #   assign(save.query.XML, query.XML, pos=.GlobalEnv)
  # }
  
  return(DF)
}


#================================================#
#' getAxDatasetFromEncodedQuery
#' 
#' @description Extracts data from Anametrix API using Base64-encoded Anametrix query, and returns R data frame
#'
#' @param auth Authentication instance. result of \code{\link{authenticate}} function
#' @param encoded.query Base64-encoded query XML corresponding to Anametrix API requirements.
#'                      Contact your product manager for assistance.
#' @param start.date Specifies start date of the query (character string, format: YYYY-MM-DD).
#' @param end.date Specifies end date of the query (character string, format: YYYY-MM-DD).
#' @param verbose Specifies whether to print out notifications. Example: FALSE
#' 
#' @family dowload/upload functions
#'
#' @return R data frame containing the observations requested from the Anametrix API table
#' @export
#'
getAxDatasetFromEncodedQuery <- function(auth, encoded.query, start.date, end.date, verbose) {
  if (!authenticationCheck(auth)) return(NULL)
  if (!apiUrlCheck(auth)) return(NULL)
  
  doc				<- xmlTreeParse(base64Decode(encoded.query))
  innerQueryXML	<- xmlRoot(doc)
  xmlAttrs(innerQueryXML[["environment"]][["dateRanges"]][["range"]])["start"] <- start.date
  xmlAttrs(innerQueryXML[["environment"]][["dateRanges"]][["range"]])["end"] <- end.date
  
  uid				<- paste("R-Module", randomAlphaNumericUID(), sep = "-")
  outerQueryXML	<- xmlNode("query", attrs = c(id = uid, resultSetMode = "stream", resultSetFormat = "tsv"),
                           namespaceDefinitions = toString(" "))
  outerQueryXML[["environment"]]	<- innerQueryXML[["environment"]]
  outerQueryXML[["query"]]		<- innerQueryXML[["query"]]
  rootLevel						<- xmlNode("query")
  rootLevel[["query"]]			<- outerQueryXML
  commandNode		<- xmlNode("executeQuery", attrs = c(token = toString(auth$authtoken)),
                          namespaceDefinitions = "http://api.anametrix.com/api-envelope.xsd")
  commandNode[["query"]]			<- rootLevel
  
  columns			<- innerQueryXML[["query"]][["columns"]]
  columnvector	<- character(0)
  for (i in 1:xmlSize(columns)) {
    columnXML		<- columns[[i]]
    if (!is.na(xmlAttrs(columnXML)["virtual"]))
      next
    displayName		<- xmlAttrs(columnXML)["displayName"]
    displayName		<- gsub("(^ +)|( +$)", "", displayName)
    columnvector	<- c(columnvector, displayName)
  }
  names(columnvector)	<- NULL
  h				<- getCurlHandle()
  reader			<- queryReader(columnvector, verbose)
  cmdNode			<- toString(commandNode)
  cmdNode			<- gsub("\\&apos;", "'", cmdNode)
  cmdNode			<- curlEscape(cmdNode)
  query.XML.req	<- paste(sep = "", "cmd=", cmdNode)
  tryCatch(
    {getURI(auth$axapiUri, ssl.verifypeer = FALSE, followLocation = TRUE,
            postfields = query.XML.req, .encoding = "UTF-8", curl = h,
            .opts = list(timeout = 3600, verbose = T),
            useragent = "R", write = chunkToLineReader(reader$read)$read,
            verbose = verbose)
    }, error = function(err) {
      cat("Error occured when retrieving data:", conditionMessage(err), "\n")
      close(reader$fileCon())
      file.remove(reader$filename())
      return(NULL)
    }, finally = {
      close(reader$fileCon())
    }
  )
  
  if (verbose)
    print("Anametrix server responding ...")
  if (file.info(reader$filename())$size > 0) {
    DF			<- read.table(reader$filename(), sep = ",", header = T)
  }
  else {
    cat("Error occured when retrieving data:  \n",
        "Make sure data suite, table, and columns are specified correctly.\n")
    file.remove(reader$filename())
    return(NULL)
  }
  
  file.remove(reader$filename())
  rm(reader)
  
  DF
}


#================================================#
#' uploadData
#' 
#' @description uploads data from R data.frame into Anametrix table
#'
#' @param auth Authentication instance. See \code{\link{authenticate}}.
#' @param report.suite.id ID number (numeric or character) of the data suite.
#' @param table.object See \code{\link{getTableConfiguration}} to get it
#' @param dataframe Data to upload. Must have the same number of columns as the table.object
#' @param columns Array of column names that need to be uploaded. Example: c("city","country","male_population")
#'                Use printTable(tableObject) to see all column names.
#' @param tag tags are used to identify the origin of the upload. Tags are associated to a user, therefore data associated with another user's tag can't be removed or modified.
#' @param truncation "none" or "tag". If "none", new data is just bound to the existing table. If "tag", data associated with the same tag is overwritten.
#' 
#' @family dowload/upload functions
#'
#' @export
uploadData <- function(auth, report.suite.id, table.object, dataframe, columns, tag, truncation = "none") {
  if (!authenticationCheck(auth)) return(NULL)
  upload.XML		<- constructUploadXML(auth, report.suite.id, table.object, dataframe, columns, tag, truncation)
  
  if (is.null(upload.XML)) {
    cat("Unable to upload data.", "\n")
    return(NULL)
  }
  query.XML.req	<- paste(sep = "", "cmd=", curlEscape(toString.XMLNode(upload.XML)))
  tryCatch({
    h <- getCurlHandle()
    resp <- getURL(auth$axapiUri, ssl.verifypeer = FALSE, postfields = query.XML.req, encoding = "UTF-8", curl = h,
                   .opts = list(timeout = 1600, verbose = TRUE), useragent = "R", verbose = F)
  }, error = function(err) {
    cat("Error while uploading data:", conditionMessage(err), "\n")
    rm(h)
    return(NULL)
  })
  print(resp)
  if (substr(resp, 17, 17) == '0') {
    cat("Upload Successful.\n")
  } else {
    cat(resp)
  }
  
  rm(h, resp)
}


#------------------------------------------------#
#           Metadata functions----
#------------------------------------------------#

#================================================#
#' getAccountInfo
#' 
#' @description Get info (name, id, description) on all accounts a user has access to
#' 
#' @param auth Authentication instance. result of \code{\link{authenticate}} function
#' 
#' @return R data frame containing one row per account 'auth' has access to (account id (\code{acct.id}), account name (\code{acct.name}), and additional info)
#' 
#' @examples 
#' username <- "demo.user@demoanametrix.com"
#' password <- "password"
#' if (!authenticationCheck(auth)) 
#' auth <- authenticate("https://demo.anametrix.com/api", username, password)
#' acct.df <- getAccountInfo(auth)
#'
#' @family metadata functions
#' @export

getAccountInfo <- function(auth){
  cui <- getCompactUserInfoXML(auth)
  result <- parseCUIForAccts(cui)
  return(result)
}

#================================================#
#' getDatasuiteInfo
#' 
#' @description Get info (name, id, description) on all datasuites a user has access to (for all accounts)
#' 
#' @param auth Authentication instance. result of \code{\link{authenticate}} function
#' 
#' @return R data frame containing one row per datasuite 'auth' has access to.
#' 
#' @examples 
#' username <- "demo.user@demoanametrix.com"
#' password <- "password"
#' if (!authenticationCheck(auth)) 
#' auth <- authenticate("https://demo.anametrix.com/api", username, password)
#' datasuites.df <- getDatasuiteInfo(auth)
#'
#' @family metadata functions
#' 
#' @export

getDatasuiteInfo <- function(auth){
  cui 		<- getCompactUserInfoXML(auth)
  result  <- parseCompactUserInfo(cui, acct.vars=c("name"), d.s.vars=c("id", "name", "description"))
  return(result)
}

#================================================#
#' getSegmentInfo
#'
#' @description List all the segments for a specific datasuite
#'
#' @param auth Authentication instance. result of \code{\link{authenticate}} function
#' @param report.suite.id ID number (as numeric or character) of datasuite. Can be obtained using the function \code{getDatasuiteInfo} (column \code{d.s.id})
#' 
#' @return R data frame containing one row per segment with segment id, segment name and the datasuite id (reportSuiteId)
#'  
#' @examples 
#' username <- "demo.user@demoanametrix.com"
#' password <- "password"
#' if (!authenticationCheck(auth))
#' auth <- authenticate("https://demo.anametrix.com/api", username, password)
#' segment.df <- getSegmentInfo(auth, 10385)
#' 
#' @family metadata functions
#' 

#' @export
getSegmentInfo <- function(auth, report.suite.id){
  seg.xml    <- getFullSegmentListXML(auth, report.suite.id)
  result     <- parseSegmentInfoByDS(seg.xml, vars=c('id', 'reportSuiteId', 'name'), prefix=NULL) 
  return(result)
}


#================================================#
#' getTableInfo
#'
#' @description List all the tables for a specific datasuite
#'
#' @param auth Authentication instance. result of \code{\link{authenticate}} function
#' @param report.suite.id ID number (as numeric or character) of datasuite. Can be obtained using the function \code{getDatasuiteInfo} (column \code{d.s.id})
#' @param vars info to download (column names of the output dataframe). \code{tableName} is the data warehouse name of the table, \code{name} is the name displayed in the UI.
#' 
#' @return R data frame containing one row per table in the datasuite report.suite.id
#' 
#' @examples 
#' username <- "demo.user@demoanametrix.com"
#' password <- "password"
#' if (!authenticationCheck(auth)) 
#' auth <- authenticate("https://demo.anametrix.com/api", username, password)
#' tables.df  <- getTableInfo(auth, report.suite.id = 10385)
#' 
#' @family metadata functions
#' 

#' @export
getTableInfo <- function(auth, report.suite.id, vars=c('id', 'name', 'menuPath', 'datasource', 'tableName', 'segmentable')) {
  if (!authenticationCheck(auth)) return(NULL)
  
  full.table.list				<- getFullTableListXML(auth, report.suite.id)
  table.list					<- full.table.list[[1]]
  
  if (length(table.list) == 0){
    return(NULL)
    break
  }
  
  suite.tables.df				<- ldply(table.list[1:length(table.list)], function(x) xmlAttrs(x)[vars])[,-1]
  suite.tables.df				<- suite.tables.df[order(suite.tables.df$name,suite.tables.df$id),]
  row.names(suite.tables.df)	<- NULL
  
  return(suite.tables.df)
}


#================================================#
#' getVariableInfo
#'
#' @description List all columns for a specific table.
#' 
#' @param table.object output from \code{\link{getTableConfiguration}} function
#' @param vars info to download (column names of the output dataframe). Default variables are \code{c('id', 'tableColumnTypeId', 'columnName', 'name')}. 
#'        Additional variable names are c('summable', 'required', 'showInUI', 'unique', 'presentationDefault', 'aggregationTypeId', 'presentationTypeId', 'decimalPlaces', 'description')
#' 
#' @return R data frame containing one row per variable in the table.
#' 
#' @examples 
#' username <- "demo.user@demoanametrix.com"
#' password <- "password"
#' if (!authenticationCheck(auth)) 
#' auth <- authenticate("https://demo.anametrix.com/api", username, password)
#' browser.table  <- getTableConfiguration(auth, 10385, "Browsers")
#' variable.df <- getVariableInfo(browser.table)
#' 
#' @family metadata functions
#' 
#' @export

getVariableInfo <- function(table.object, vars=c('id', 'tableColumnTypeId', 'columnName', 'name', 
                                                 'summable', 'required', 'showInUI', 'unique', 'presentationDefault', 'aggregationTypeId',
                                                 'presentationTypeId', 'decimalPlaces', 'description')) {
  prefix=NULL
  vars				<- match.arg(vars, several.ok=TRUE)
  if (is.null(prefix))
    var.names			<- vars
  else
    var.names			<- paste(prefix, vars, sep=".")
  
  len					<- xmlSize(table.object$tableXML[[1]])
  if (len > 0) {
    variables			<- ldply(table.object$tableXML[[1]][1:len], function(x) data.frame(t(xmlAttrs(x))))[,-1]
    positions			<- match(intersect(vars, names(variables)), vars)
    variables			<- variables[, vars[positions]]
    names(variables)	<- var.names[positions]
  } else {
    variables			<- data.frame(matrix(NA, nrow=1, ncol=length(var.names)))
    names(variables)	<- var.names
  }
  
  return(variables)
}




#================================================#
#' getDetailedInfo
#'
#' @description Get all detailed matadata info for one account (datasuites/tables/column names level). 
#' 
#' @param auth Authentication instance. result of \code{\link{authenticate}} function
#' @param account.name Name of the account for which to get metadata (column \code{acct.name} from the result of the function \code{getAccountInfo})
#' 
#' @return list of dataframes with detailed metadata. One element per datasuite of account.name.
#'
#' @examples 
#' username <- "demo.user@demoanametrix.com"
#' password <- "password"
#' if (!authenticationCheck(auth)) 
#' auth <- authenticate("https://demo.anametrix.com/api", username, password)
#' acct.detailed.info <- getDetailedInfo(auth, account.name = "Demo")
#' 
#' @family metadata functions
#' 
#' @export

getDetailedInfo <- function(auth, account.name) {
  result <- getThenParseTableInfoXML(auth, account.name, table.vars=c('id', 'name', 'menuPath', 'datasource', 'tableName'), 
                                     column.vars=c('tableColumnTypeId', 'columnName', 'name'))
  return(result)
}


#================================================#
#' getReplacementNames
#'
#' @description Get all replacement names associated to an account. Replacement names are column names displayed in the tables in the UI that were not the original names defined.
#' 
#' @param auth Authentication instance. result of \code{\link{authenticate}} function
#' @param account.name Name of the account for which to get metadata (column \code{acct.name} from the result of the function \code{getAccountInfo})
#' 
#' @return dataframe with the column names of the variables that have a replacement name in the UI, along with the associated table and datasuite. NA is no column names has replacement names.
#'
#' @examples 
#' username <- "demo.user@demoanametrix.com"
#' password <- "password"
#' if (!authenticationCheck(auth)) 
#' auth <- authenticate("https://demo.anametrix.com/api", username, password)
#' rep.names.df <- getReplacementNames(auth, account.name = "Demo")
#' 
#' @family metadata functions
#' 
#' @export
#' 

getReplacementNames <- function(auth, account.name) {
  x			<- getCompactUserInfoXML(auth)
  y			<- parseCompactUserInfo(x, acct.vars=c("name"), acct.prefix="acct", d.s.vars=c("id", "name"), d.s.prefix="d.s", var.maps=TRUE, include.accts=FALSE, rbind.results=FALSE)
  if (is.element(account.name, names(y))) {
    z			<- y[[account.name]]
    return(z)
  } else {
    cat("account.name is not authorized. Check spelling or user rights. \n")
    return(NULL)
  }
}
