#================================================#
#
# Title:		Hidden functions
#
#================================================#

# Internal functions used inside the exported functions of the anametrix package.

#================================================#
# Load packages

#' @importFrom plyr aaply adply alply colwise daply ddply join_all laply ldply llply 
#' @import RCurl
#' @import XML
#' @import dplyr
#' @importFrom stats na.omit
#' @importFrom utils  read.table type.convert write.table

#------------------------------------------------#
#      Authentication functions/checks ----                
#------------------------------------------------#

#================================================#
# isAuthTokenValid
isAuthTokenValid <- function(auth, verbose = F) {
  escaped.token      <- curlEscape(auth$authtoken)
  query.XML           <- newXMLNode("validateAuthenticationToken",
                                    attrs = c(token = toString(escaped.token)),
                                    namespaceDefinitions = "http://api.anametrix.com/api-envelope.xsd")
  
  query.XML.req        <- paste("cmd=", toString.XMLNode(query.XML), sep = "")
  
  if (verbose) cat("Check token validity ... ")
  
  resp <- getURL(auth$axapiUri,
                 ssl.verifypeer = FALSE,
                 postfields = query.XML.req,
                 .opts = list(timeout = 100),
                 useragent = "R")
  # if resp == 0 : token valid ; if resp == 2 : token invalid
  if (grepl("0", resp)) {
    if (verbose) cat("valid token.\n")
    return(TRUE)
  }
  
  if (verbose) cat("invalid token.\n")
  return(FALSE)
}

#================================================#
# apiUrlCheck, apiUriCheck
apiUrlCheck <- apiUriCheck <- function(auth) {# Notes:  for use after authenticate function
  if (is.null(auth) || is.null(auth$axapiUri) || as.character(auth$axapiUri) == "") {
    cat("API Endpoint URL is missing; specify API Endpoint URL and get new authentication credentials:  \n\n")
    cat("	auth <- authentication(axapi.uri, username, password)", "\n\n")
    invisible(FALSE)
  } else
    invisible(TRUE)
}

#------------------------------------------------#
#       Download/upload  functions----
#------------------------------------------------#

#================================================#
# getDBDateFormat
getDBDateFormat <- function(unit.of.time) {
  new.date.format		<- ifelse(is.element(casefold(unit.of.time), c("hour", "hours", "hr", "hrs")),
                             "yyyy-MM-dd HH:mm:ss",
                             ifelse(is.element(casefold(unit.of.time), c("day", "days")),
                                    "yyyy-MM-dd",
                                    ifelse(is.element(casefold(unit.of.time), c("week", "weeks", "wk", "wks")),
                                           "yyyy-MM-dd",
                                           ifelse(is.element(casefold(unit.of.time), c("month", "months", "mo", "mos")),
                                                  "yyyy-MM-01",
                                                  ifelse(is.element(casefold(unit.of.time), c("year", "years", "yr", "yrs")),
                                                         "yyyy-01-01",
                                                         unit.of.time)))))
  
  return(new.date.format)
}

#================================================#
# getDBDateText
getDBDateText <- function(unit.of.time) {
  new.date.text		<- ifelse(is.element(casefold(unit.of.time), c("hour", "hours", "hr", "hrs")),
                           "hour",
                           ifelse(is.element(casefold(unit.of.time), c("day", "days")),
                                  "day",
                                  ifelse(is.element(casefold(unit.of.time), c("week", "weeks", "wk", "wks")),
                                         "week",
                                         ifelse(is.element(casefold(unit.of.time), c("month", "months", "mo", "mos")),
                                                "month",
                                                ifelse(is.element(casefold(unit.of.time), c("year", "years", "yr", "yrs")),
                                                       "year",
                                                       unit.of.time)))))
  
  return(new.date.text)
}

#================================================#
# getFullSegmentListXML
getFullSegmentListXML <- function(auth, report.suite.id) {
  if (!authenticationCheck(auth)) return(NULL)
  
  escaped.token			<- curlEscape(auth$authtoken)
  segmentlistXMLreq		<- paste("cmd=\t<getSegmentList xmlns=\"http://api.anametrix.com/api-envelope.xsd\" token=\"",
                              escaped.token, "\">\n  \t\t\t\t\t\t\t\t\t<reportSuite>",
                              report.suite.id, "</reportSuite>\n\t\t\t\t\t\t\t\t</getSegmentList>", sep = "")
  resp					<- getURL(auth$axapiUri, ssl.verifypeer = FALSE, postfields = segmentlistXMLreq,
                     .opts = list(timeout = 100))
  doc						<- xmlTreeParse(resp)
  root					<- xmlRoot(doc)
  
  rm(doc)
  rm(escaped.token)
  rm(resp)
  
  return(root)
}

#================================================#
# getFullTableListXML
getFullTableListXML <- function(auth, report.suite.id) {
  if (!authenticationCheck(auth)) return(NULL)
  
  escaped.token			<- curlEscape(auth$authtoken)
  tablelistXMLreq			<- paste("cmd=\t<getTableList xmlns=\"http://api.anametrix.com/api-envelope.xsd\" token=\"",
                             escaped.token, "\">\n  \t\t\t\t\t\t\t\t\t<reportSuite>",
                             report.suite.id, "</reportSuite>\n\t\t\t\t\t\t\t\t</getTableList>", sep = "")
  resp					<- getURL(auth$axapiUri, ssl.verifypeer = FALSE, postfields = tablelistXMLreq,
                     .opts = list(timeout = 100))
  doc						<- xmlTreeParse(resp)
  root					<- xmlRoot(doc)
  
  rm(doc)
  rm(escaped.token)
  rm(resp)
  
  return(root)
}

#================================================#
# getSegmentInfoXML
getSegmentInfoXML <- function(auth, account.name=NULL, data.suite.ids=NULL, one.acct.only=FALSE, cui=NULL) {
  if (!authenticationCheck(auth)) return(NULL)
  
  if (is.null(account.name) & is.null(data.suite.ids))
    stop("At least one of account.name or data.suite.ids must be non-null")
  
  if (is.null(data.suite.ids)) {
    if (is.null(cui))
      cui				<- getCompactUserInfoXML(auth)
    
    if (length(cui) > 0) {
      names			<- laply(cui[[1]][[1]][1:length(cui[[1]][[1]])], function(x) xmlAttrs(x)['name'])
    } else {
      cat("Error when attempting to retrieve table information \n")
      cat("    Try rerunning authentication function.\n")
      cat("    new.auth	<- authentication(axapi.uri, username, password)", "\n\n")
      stop()
    }
    
    if (is.element(account.name, names)) {
      position		<- match(account.name, names)
      acct			<- cui[[1]][[1]][[position]]
      data.suite.ids	<- as.vector(parseCUIForDataSuites(acct, c('id')))
    } else {
      cat("Available account names:  \n")
      print(cbind(names))
      warning("Value of account.name argument is not among available accounts.")
      invisible(NULL)
    }
  }
  
  if (length(data.suite.ids) > 0) {
    cat("Getting segment information ...")
    data.suites			<- llply(unlist(data.suite.ids), function(x, auth) getFullSegmentListXML(auth, x), auth=auth)
    if (all(laply(data.suites, length) == 0)) {
      cat("All segment lists are empty:  \n")
      cat("    Check data suite ID numbers or try rerunning authentication function.\n")
      stop()
    }
    cat(" done.\n")
    if (length(data.suite.ids) > 1 & !is.null(data.suite.ids))
      warning("Segment information returned comes from more than one data suite.")
  }
  names(data.suites)	<- unlist(data.suite.ids)
  
  return(data.suites)
}

#================================================#
# getTableInfoXML
getTableInfoXML <- function(auth, account.name=NULL, data.suite.ids=NULL, one.acct.only=FALSE, cui=NULL) {
  if (!authenticationCheck(auth)) return(NULL)
  
  if (is.null(account.name) & is.null(data.suite.ids))
    stop("At least one of account.name or data.suite.ids must be non-null")
  
  if (is.null(data.suite.ids)) {
    if (is.null(cui))
      cui				<- getCompactUserInfoXML(auth)
    
    if (length(cui) > 0) {
      names			<- laply(cui[[1]][[1]][1:length(cui[[1]][[1]])], function(x) xmlAttrs(x)['name'])
    } else {
      cat("Error when attempting to retrieve table information \n")
      cat("    Try rerunning authentication function.\n")
      cat("    new.auth	<- authentication(axapi.uri, username, password)", "\n\n")
      stop()
    }
    
    if (is.element(account.name, names)) {
      position		<- match(account.name, names)
      acct			<- cui[[1]][[1]][[position]]
      data.suite.ids	<- as.vector(parseCUIForDataSuites(acct, c('id')))
    } else {
      print("Available account names:  ")
      print(cbind(names))
      warning("Value of account.name argument is not among available accounts.")
      return(NULL)
    }
  }
  
  if (length(data.suite.ids) > 0) {
    cat("Getting table information ...")
    data.suites			<- llply(unlist(data.suite.ids), function(x, auth) getFullTableListXML(auth, x), auth=auth)
    if (all(laply(data.suites, length) == 0)) {
      cat("All data suites are empty:  \n")
      cat("    Check data suite ID numbers or try rerunning authentication function.\n")
      stop()
    }
    cat(" done.\n")
    if (length(data.suite.ids) > 1 & !is.null(data.suite.ids))
      warning("Table information returned comes from more than one data suite.")
  }
  names(data.suites)	<- unlist(data.suite.ids)
  
  return(data.suites)
}

#================================================#
# getColumnTypeId
getColumnTypeId <- function(c.name, table.object) {
  column.list.xml		<- table.object$tableXML[[1]]
  for (i in 1:xmlSize(column.list.xml)) {
    column.xml			<- column.list.xml[[i]]
    column.name			<- xmlAttrs(column.xml)["columnName"]
    column.type.id		<- as.numeric(xmlAttrs(column.xml)["tableColumnTypeId"])
    if (as.character(column.name) == c.name)
      return(column.type.id)
  }
  
  rm(i)
  rm(column.name)
  
  return(column.type.id)
}

#================================================#
# constructQueryXML
constructQueryXML <- function(auth, report.suite.id, table.object, segment.id, columns, filter.def, start.date, end.date, max.rows,
                              unit.of.time, sort.column, sort.direction, date.format.override=FALSE, return.as.date.selection=NULL) {
  
  if (!authenticationCheck(auth)) return(NULL)
  if (!apiUrlCheck(auth)) return(NULL)
  
  escaped.token		<- auth$authtoken
  root				<- newXMLNode("executeQuery", attrs = c(token = toString(escaped.token)),
                        namespaceDefinitions = "http://api.anametrix.com/api-envelope.xsd")
  level1				<- newXMLNode("query", parent = root)
  uid					<- paste("R-Module", randomAlphaNumericUID(), sep = "-")
  level2				<- newXMLNode("query", attrs = c(id = uid, resultSetMode = "stream", resultSetFormat = "tsv"),
                          namespaceDefinitions = toString(" "), parent = level1)
  level3				<- newXMLNode("environment", parent = level2)
  date.ranges			<- newXMLNode("dateRanges", parent = level3)
  newXMLNode("range", attrs = c(start = toString(start.date),
                                end = toString(end.date)), parent = date.ranges)
  report.suite		<- newXMLNode("reportSuite", report.suite.id, parent = level3)
  
  if (is.null(segment.id)) {
    query				<- newXMLNode("query", attrs = c(minGranularity = "hour",
                                              table = toString(xmlAttrs(table.object$tableXML)["tableName"]),
                                              dateColumn = "date", start = "0", count = max.rows), parent = level2)
  } else {
    query				<- newXMLNode("query", attrs = c(minGranularity = "hour",
                                              table = toString(xmlAttrs(table.object$tableXML)["tableName"]),
                                              dateColumn = "date", start = "0", count = max.rows,
                                              segment = toString(segment.id)), parent = level2)
  }
  
  columns.xml			<- newXMLNode("columns", parent = query)
  m.index				<- 1
  c.index				<- 1
  table.column.type.id	<- 1
  date.format			<- getDBDateFormat(unit.of.time)
  unit.of.time		<- as.character(getDBDateText(unit.of.time))
  return.as.date		<- unit.of.time == "hour" || unit.of.time == "day" ||
    unit.of.time == "month" || unit.of.time == "year"
  sort.column.internal.name		<- "m_1"
  if (!is.null(return.as.date.selection)) {
    return.as.date		<- as.logical(return.as.date.selection)
  }
  if (date.format.override) {
    date.format			<- unit.of.time
  }
  
  for (column.name in columns) {
    table.column.type.id		<- getColumnTypeId(column.name, table.object)
    if (table.column.type.id == 2) {
      internalname			<- paste(sep = "_", "m", m.index)
      m.index					<- m.index + 1
    }
    else {
      internalname			<- paste(sep = "_", "c", c.index)
      c.index					<- c.index + 1
    }
    if (tolower(as.character(column.name)) == "date" || table.column.type.id == 5) {
      newXMLNode("column", attrs = c(name = internalname, columnName = toString(column.name),
                                     returnAsDate = tolower(return.as.date), dateFormat = date.format), parent = columns.xml)
    } else {
      newXMLNode("column", attrs = c(name = internalname, columnName = toString(column.name)),
                 parent = columns.xml)
    }
    if (as.character(column.name) == sort.column) {
      sort.column.internal.name		<- internalname
    }
  }
  
  sort.order.xml			<- newXMLNode("sortOrder", parent = query)
  sort.order.column.xml	<- newXMLNode("column", attrs = c(name = sort.column.internal.name,
                                                          order = toupper(sort.direction)), parent = sort.order.xml)
  
  # add general filter node
  filter.type.xref <- data.frame(display.name = c("equal", "equals", "eq", "=", "==", "not equal to", "not equal", "not eq to", "not eq", "!=", "<>", "greater or equal to", "greater than or equal to", "greater or equal", "greater than or equal", "gt or eq to", "gt or eq", ">=", "greater", "greater than", "gt", ">", "less or equal to", "less than or equal to", "less or equal", "less than or equal", "lt or eq to", "lt or eq", "<=", "less", "less than", "lt", "<", "contains", "contain", "does contain", "doesn't contain", "not contain", "starts with", "does start with", "start with", "starts", "start", "ends with", "does end with", "end with", "ends", "end", "does not start with", "does not start", "doesn't start with", "doesn't start", "not start with", "not start", "does not end with", "does not end", "doesn't end with", "doesn't end", "not end with", "not end", "in this list", "in list", "in", "not in this list", "not in list", "not in", "has a value", "has value", "not empty", "is not empty", "not null", "is not null", "has no value", "no value", "empty", "is empty", "null", "is null"),
                                 xml.name = c("equal", "equal", "equal", "equal", "equal", "notEqual", "notEqual", "notEqual", "notEqual", "notEqual", "notEqual", "greaterThanEqual", "greaterThanEqual", "greaterThanEqual", "greaterThanEqual", "greaterThanEqual", "greaterThanEqual", "greaterThanEqual", "greaterThan", "greaterThan", "greaterThan", "greaterThan", "lessThanEqual", "lessThanEqual", "lessThanEqual", "lessThanEqual", "lessThanEqual", "lessThanEqual", "lessThanEqual", "lessThan", "lessThan", "lessThan", "lessThan", "like", "like", "like", "notLike", "notLike", "like", "like", "like", "like", "like", "like", "like", "like", "like", "like", "notLike", "notLike", "notLike", "notLike", "notLike", "notLike", "notLike", "notLike", "notLike", "notLike", "notLike", "notLike", "in", "in", "in", "notIn", "notIn", "notIn", "notNull", "notNull", "notNull", "notNull", "notNull", "notNull", "isNull", "isNull", "isNull", "isNull", "isNull", "isNull"),
                                 num.values = c(1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 2, 2, 2, 2, 2, 2, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0),
                                 stringsAsFactors = FALSE)
  
  if (!is.null(filter.def$column)) {
    filter.column		<- filter.def$column
    filter.type			<- subset(filter.type.xref, filter.type.xref$display.name == tolower(filter.def$type))$xml.name
    filter.value		<- filter.def$value
    
    if (length(filter.type) == 0 | length(filter.column) == 0 | length(filter.value) == 0){
      print("filter not applied because not well defined")
    } else {
      filterXML			<- newXMLNode("filters", parent = query)
      filterXML.level1	<- newXMLNode("filter", attrs = c(type = "where"), parent = filterXML)
      filterXML.level2	<- newXMLNode("and", parent = filterXML.level1)
      filterXML.level3	<- newXMLNode("and", parent = filterXML.level2)
      filterXML.level4	<- newXMLNode(filter.type, parent = filterXML.level3)
      filterXML.level5	<- newXMLNode("key", filter.column, parent = filterXML.level4)
      for (i in 1 : length(filter.value)){
        newXMLNode("value", filter.value[i], parent = filterXML.level4)
      }
    }
  }
  
  # return xml query
  return(root)
}


#================================================#
# constructUploadXML
constructUploadXML <- function(auth, report.suite.id, table.object, dataframe, columns, tag, truncation) {
  if (!authenticationCheck(auth)) return(NULL)
  
  # escaped.token	<- curlEscape(auth$authtoken)
  escaped.token	<- auth$authtoken
  root			<- newXMLNode("uploadData", attrs = c(token = toString(escaped.token)),
                       namespaceDefinitions = "http://api.anametrix.com/api-envelope.xsd")
  properties		<- newXMLNode("properties", parent = root)
  newXMLNode("reportSuite", report.suite.id, parent = properties)
  
  if (!is.null(tag))
    newXMLNode("tag", tag, parent = properties)
  
  currentDate		<- format(Sys.time(), "%Y-%m-%d")
  newXMLNode("date", currentDate, parent = properties)
  
  if (is.null(table.object$tableXML) || as.character(table.object$tableXML) == "") {
    cat("Unable to upload data. Table not specified.", "\n")
    return(NULL)
  }
  
  newXMLNode("table", xmlAttrs(table.object$tableXML)["tableName"], parent = properties)
  
  if (!is.null(truncation)) {
    newXMLNode("truncation", truncation, parent = properties)
  } else {
    newXMLNode("truncation", "FALSE", parent = properties)
  }
  
  level1			<- newXMLNode("data", parent = root)
  rows			<- list()
  if (nrow(dataframe) > 0) rows <- apply(dataframe, MARGIN = 1, function(r) newXMLNode("row", attrs = sub("^ +", "", r)))
  rowsparent		<- newXMLNode("data", namespaceDefinitions = toString(" "), parent = level1, .children = rows)
  rm(rows)
  
  return(root)
}

#================================================#
# encodedQueryToXML
encodedQueryToXML <- function(encoded.query) {
  return(xmlTreeParse(base64Decode(encoded.query)))
}

#------------------------------------------------#
#           Metadata functions----
#------------------------------------------------#

#================================================#
# getThenParseTableInfoXML
getThenParseTableInfoXML <- function(auth, account.name=NULL, data.suite.ids=NULL, one.acct.only=FALSE, cui=NULL,
                                     table.vars=c('id', 'name', 'menuPath', 'datasource', 'tableName', 'schemaName',
                                                  'tableTypeId', 'showInUI', 'description', 'datePartitioned',
                                                  'segmentable', 'streamingDataCollection', 'globallyPublished'),
                                     table.prefix="table",
                                     column.vars=c('id', 'tableColumnTypeId', 'columnName', 'name', 'description',
                                                   'summable', 'required', 'showInUI', 'unique', 'presentationDefault',
                                                   'aggregationTypeId', 'presentationTypeId', 'decimalPlaces'),
                                     column.prefix=NULL,
                                     d.s.prefix="d.s", rbind.results=FALSE, rbind.inner.results=TRUE) {
  
  # For each data suite in an account (or vector of data.suite.ids), get full table list information from 'getFullTableListXML' function.
  # Parse list of table information from multiple data suites, e.g. all data suites in an account, in a list of data.frame objects
  #	Returns table information in a list of data.frame objects or a single combined data.frame object
  #
  # One of acct.name or data.suite.ids is required. If both acct.name and data.suite.ids are provided then data.suite.ids is used
  
  # auth user authentication information from 'authenticate' function
  # account.name name of account used in XML code
  # data.suite.ids ID numbers (as numeric or character) for data suites, can be from different accounts
  # one.acct.only restricts data suites to only the account designated by account.name (not implemented)
  # cui object returned by function getCompactUserInfoXML
  # table.vars possible variables containing table information
  # table.prefix text string to add to the front of each table variable in order to prevent possible duplications
  # column.vars possible variables containing column information
  # column.prefix text string to add to the front of each column variable in order to prevent possible duplications (default is NULL)
  # d.s.prefix text string to add to the front of each data suite variable in order to prevent possible duplications
  # rbind.results if TRUE and rbind.inner.results is TRUE, then a data.frame of all results is returned, otherwise a list is returned (default is FALSE)
  # rbind.inner.results if TRUE, then the information for each data suite is in a single data.frame object, otherwise each data suite is a list of one data frame for each table (default is TRUE)
  #
  # return table information in a list of data.frame objects or a single combined data.frame object
  
  if (!authenticationCheck(auth)) return(NULL)
  if (is.null(account.name) & is.null(data.suite.ids))
    stop("At least one of account.name or data.suite.ids must be non-null")
  
  if (is.null(data.suite.ids)) {
    if (is.null(cui))
      cui				<- getCompactUserInfoXML(auth)
    
    if (length(cui) > 0) {
      names			<- laply(cui[[1]][[1]][1:length(cui[[1]][[1]])], function(x) xmlAttrs(x)['name'])
    } else {
      cat("Error when attempting to retrieve table information \n")
      cat("    Try rerunning authentication function.\n")
      cat("    new.auth	<- authentication(axapi.uri, username, password)", "\n\n")
      stop()
    }
    
    if (is.element(account.name, names)) {
      position		<- match(account.name, names)
      acct.cui		<- cui[[1]][[1]][[position]]
      data.suite.ids	<- as.vector(parseCUIForDataSuites(acct.cui, c('id')))
    } else {
      print("Available account names:  ")
      print(cbind(names))
      warning("Value of account.name argument is not among available accounts.")
      return(NULL)
    }
  }
  
  if (length(data.suite.ids) > 0 && !is.na(unlist(data.suite.ids)[1])) {
    cat("Getting table information ...")
    data.suites			<- llply(unlist(data.suite.ids),
                           function(x, auth, tvars, tpre, cvars, cpre, dpre, rb.res, rb.inres)
                             parseTableInfoByAcct(getFullTableListXML(auth, x), tvars, tpre,
                                                  cvars, cpre, dpre, rb.res, rb.inres),
                           auth=auth, tvars=table.vars, tpre=table.prefix, cvars=column.vars,
                           cpre=column.prefix, dpre=d.s.prefix, rb.res=rbind.results,
                           rb.inres=rbind.inner.results)
    if (all(laply(data.suites, length) == 0)) {
      cat("No data retrieved from any data suite:  \n")
      cat("    Check data suite ID numbers or try rerunning authentication function.\n")
      stop()
    }
    cat(" done.\n")
    if (!is.null(data.suite.ids) && length(data.suite.ids) > 1)
      warning("Table information returned comes from more than one data suite.")
  } else {
    message(paste("Account, ", account.name, ", contains no data suites.", sep=""))
    return(NULL)
  }
  names(data.suites)	<- unlist(data.suite.ids)
  
  if (rbind.results & rbind.inner.results) {
    if (is.null(d.s.prefix))
      id.name				<- "id"
    else
      id.name				<- paste(d.s.prefix, "id", sep=".")
    data.suites			<- llply(names(data.suites),
                           function(x, name) {
                             x.df <- cbind(rep(x, nrow(data.suites[[x]])), data.suites[[x]]);
                             names(x.df)[1] <- name; return(x.df) },
                           name=id.name)
    if (length(data.suites) > 1) {
      data.suites			<- na.omit(do.call('rbind', data.suites))
    } else
      data.suites			<- data.suites[[1]]
  }
  
  return(data.suites)
}

#================================================#
# queryReader
queryReader <- function(columns, verbose) {
  i				<- 1
  batchsize		<- 1
  needToStore		<- FALSE
  filename <- tempfile(pattern = "file", tmpdir = tempdir(), fileext = ".txt")
  fileCon <- file(filename, "wt")
  
  read <- function(chunk) {
    con				<- textConnection(chunk)
    on.exit(close(con))
    
    while (length(oneLine	<- readLines(con, n = batchsize, warn = TRUE, ok = TRUE)) > 0) {
      if (needToStore) {
        tryCatch({
          c.ol			<- textConnection(oneLine)
          tempDF <- read.table(c.ol, header = FALSE, sep="\t", quote = "", comment.char = "",
                               as.is = TRUE, blank.lines.skip = TRUE, numerals = "no.loss")              # numerals = no.loss permits to avoid errors when reading if which look like number with many digits
          if (ncol(tempDF) == length(columns)) {
            if (i == 1) {
              names(tempDF)		<- columns
              write.table(tempDF, file = fileCon, col.names = TRUE, row.names = FALSE,
                          qmethod = "escape",sep="\t")
            } else {
              write.table(tempDF, file = fileCon, col.names = FALSE, row.names = FALSE,
                          qmethod = "escape",sep="\t")
            }
            i <<- i + 1
          }
          rm(tempDF)
          close(c.ol)
        }, error = function(err) {
          if (verbose) {
            cat("Error when parsing line:", conditionMessage(err), "\n")
            cat("Corrupted line:", oneLine, "\n")
            cat("\n")
          }
        })
      } else if (as.character(oneLine) == "</result>") {
        needToStore <<- TRUE
        batchsize <<- 200
      }
    }
  }
  
  list(read = read, fileCon = function() fileCon, filename = function() filename)
}



#================================================#
# randomAlphaNumericUID
randomAlphaNumericUID <- function(n = 1, length = 10) {
  random.string		<- c(1:n)
  for (i in 1:n) {
    random.string[i]		<- paste(sample(c(0:9, letters, LETTERS), length, replace = TRUE), collapse = "")
  }
  return(toupper(random.string))
}



#================================================#
# parseCUIForAccts
parseCUIForAccts <- function(cui, vars=c('id', 'nid', 'name', 'description', 'language', 'skin', 'admin'),
                             prefix="acct") {
  
  # Get account information for all accounts accessible by user, i.e. output from 'getCompactUserInfoXML' function
  # Params
  #    cui output from 'getCompactUserInfoXML' function
  #    vars possible variables containing account information
  #    prefix text string added to front of each variable name to prevent possible duplications with variables at other levels
  # Return
  #    data.frame object with account information for all accounts in the cui argument
  if (is.null(prefix))
    var.names		<- vars
  else
    var.names		<- paste(prefix, vars, sep=".")
  
  len				<- length(cui[[1]][[1]])
  accounts		<- ldply(cui[[1]][[1]][1:len], function(x) data.frame(t(xmlAttrs(x)[vars])))[,-1,drop=FALSE]
  #	accounts		<- ldply(cui[[1]][[1]][1:len], function(x) data.frame(t(xmlAttrs(x)[vars])))[,-1]
  #	if (is.null(dim(accounts)))
  #		accounts		<- data.frame(accounts)
  names(accounts)	<- var.names
  
  return(accounts)
}

#================================================#
# parseCUIForDataSuites
parseCUIForDataSuites <- function(acct, vars=c('id', 'nid', 'name', 'description', 'timezone', 'timezoneOffset', 'admin'), prefix="d.s") {
  # Get data suite information for an account
  # Params
  #    acct one account's worth of output from 'getCompactUserInfoXML' function, i.e. cui[[1]][[1]][[k]] for kth account information
  #    vars possible variables containing data suite information
  #    prefix text string added to front of each variable name to prevent possible duplications with variables at other levels
  # Return
  #    data.frame object with data suite information for all data suites in an account
  
  
  if (is.null(prefix))
    var.names		<- vars
  else
    var.names		<- paste(prefix, vars, sep=".")
  
  suite.pos		<- which(names(acct) == "reportSuites")
  if (length(suite.pos) > 0) {
    report.suites	<- acct[[suite.pos]]
    num.suites		<- length(report.suites)
    
    if (num.suites > 0)
      suite.attrs		<- ldply(report.suites[1:num.suites], function(x) data.frame(t(xmlAttrs(x)[vars])))[,-1,drop=FALSE]
    #			suite.attrs		<- ldply(report.suites[1:num.suites], function(x) data.frame(t(xmlAttrs(x)[vars])))[,-1]
    else
      suite.attrs		<- data.frame(matrix(NA, nrow=1, ncol=length(var.names)))
    
    #		if (is.null(dim(suite.attrs)))
    #			suite.attrs			<- data.frame(suite.attrs)
    names(suite.attrs)	<- var.names
    
  } else
    suite.attrs		<- data.frame(matrix(NA, nrow=1, ncol=length(var.names)))
  
  return(suite.attrs)
}

#================================================#
# getCompactUserInfoXML
getCompactUserInfoXML <- function(auth) {
  #	List all compact user info to which the user (defined by 'auth' argument) has access
  #	Includes the replacement (or display) names for the internal column names
  
  if (!authenticationCheck(auth)) return(NULL)
  
  cat("Getting compact user info ...")
  escaped.token	<- curlEscape(auth$authtoken)
  request			<- paste('cmd=<getCompactUserInfo xmlns="http://api.anametrix.com/api-envelope.xsd" token="',
                     escaped.token, "\"></getCompactUserInfo>", sep="")
  resp			<- getURL(auth$axapiUri, ssl.verifypeer = FALSE, postfields = request, .opts = list(timeout = 100))
  doc				<- xmlTreeParse(resp)
  cui				<- xmlRoot(doc)
  
  if (length(cui) == 0) {
    cat("\nNo information retrieved:  \n")
    cat("    Try rerunning authentication function.\n")
    stop()
  }
  cat(" done.\n")
  
  rm(doc)
  rm(escaped.token)
  rm(resp)
  
  return(cui)
}

#================================================#
# parseTableInfoByAcct
parseTableInfoByAcct <- function(t.info.acct,
                                 table.vars=c('id', 'name', 'menuPath', 'datasource', 'tableName', 'schemaName',
                                              'tableTypeId', 'showInUI', 'description', 'datePartitioned',
                                              'segmentable', 'streamingDataCollection', 'globallyPublished'),
                                 table.prefix="table",
                                 column.vars=c('id', 'tableColumnTypeId', 'columnName', 'name', 'description',
                                               'summable', 'required', 'showInUI', 'unique', 'presentationDefault',
                                               'aggregationTypeId', 'presentationTypeId', 'decimalPlaces'),
                                 column.prefix=NULL, d.s.prefix="d.s", rbind.results=FALSE, rbind.inner.results=TRUE) {
  # Parse list of table information from multiple data suites, e.g. all data suites in an account.
  # Function used in getThenParseTableInfoXML.
  # Params
  #    t.info.acct output from 'getTableInfoXML' function
  #    table.vars possible variables containing table information
  #    table.prefix text string to add to the front of each table variable in order to prevent possible duplications
  #    column.vars possible variables containing column information
  #    column.prefix text string to add to the front of each column variable in order to prevent possible duplications (default is NULL)
  #    d.s.prefix text string to add to the front of each data suite variable in order to prevent possible duplications
  #    rbind.results if TRUE and rbind.inner.results is TRUE, then a data.frame of all results is returned, otherwise a list is returned (default is FALSE)
  #    rbind.inner.results if TRUE, then the information for each data suite is in a single data.frame object, otherwise each data suite is a list of one data frame for each table (default is TRUE)
  # Return
  #    table information in a list of data.frame objects or a single combined data.frame object
  
  if (class(t.info.acct)[1] == "XMLNode") {
    if (xmlAttrs(t.info.acct)[1] != 0) {
      cat("No XML code available in t.info.acct object; getFullTableListXML was unsuccessful.\n")
      return(NULL)
    } else if (names(t.info.acct)[1] == "tables") {
      d.s.parsed			<- parseTableInfoByDS(t.info.acct, table.vars=table.vars, table.prefix=table.prefix,
                                         column.vars=column.vars, column.prefix=column.prefix,
                                         rbind.results=rbind.inner.results)
      rbind.results		<- FALSE
    }
  } else if (class(t.info.acct)[1] == "list") {
    with.tables			<- which(laply(t.info.acct, function(x) length(x[[1]])) > 0)
    d.s.parsed			<- llply(t.info.acct[with.tables], parseTableInfoByDS, table.vars=table.vars,
                          table.prefix=table.prefix, column.vars=column.vars,
                          column.prefix=column.prefix, rbind.results=rbind.inner.results)
  } else {
    cat("Argument t.info.acct must be an object, or list of objects, from the getFullTableListXML function\n")
    return(NULL)
  }
  
  if (rbind.results & rbind.inner.results) {
    if (is.null(d.s.prefix))
      id.name				<- "id"
    else
      id.name				<- paste(d.s.prefix, "id", sep=".")
    d.s.parsed			<- llply(names(d.s.parsed),
                          function(x, name) {
                            x.df <- cbind(rep(x, nrow(d.s.parsed[[x]])), d.s.parsed[[x]]);
                            names(x.df)[1] <- name; return(x.df) },
                          name=id.name)
    d.s.parsed			<- na.omit(do.call('rbind', d.s.parsed))
  }
  
  return(d.s.parsed)
}

#================================================#
# parseTableInfoByDS
parseTableInfoByDS <- function(t.info.d.s, table.vars=c('id', 'name', 'menuPath', 'datasource', 'tableName',
                                                        'schemaName', 'tableTypeId', 'showInUI', 'description', 'datePartitioned',
                                                        'segmentable', 'streamingDataCollection', 'globallyPublished'),
                               table.prefix="table",
                               column.vars=c('id', 'tableColumnTypeId', 'columnName', 'name', 'description',
                                             'summable', 'required', 'showInUI', 'unique', 'presentationDefault',
                                             'aggregationTypeId', 'presentationTypeId', 'decimalPlaces'),
                               column.prefix=NULL, rbind.results=FALSE) {
 
  #----#
  #	For one data suite, put information about tables & variables (from 'getFullTableListXML' function) into a list of data frames or a single data.frame object
  # Function used in getThenParseTableInfoXML.
  # Parameters
  #	    t.info.d.s	one tables-worth of output from 'getFullTableListXML' function, i.e. wrap this in llply
  #	    table.vars	possible variables containing table information, if NULL then no table variables included
  #	    table.prefix	text string to add to the front of each table variable in order to prevent possible duplications
  #	    column.vars	possible variables containing column information, if NULL then no column variables included
  #	    column.prefix	text string to add to the front of each column variable in order to prevent possible duplications (default is NULL)
  #   	rbind.results	if TRUE and rbind.inner.results is TRUE, then a data.frame of all results is returned, otherwise a list is returned (default is FALSE)
  #	Returns 
  #    a list of data.frame objects or a single data.frame object with table and column information for tables in data suite
  
   expandInfo <- function(x, y1, y2) {
    if (is.null(dim(y2))) {
      x.out			<- cbind(y1[rep(x, nrow(y2[[x]])), ], y2[[x]])
      names(x.out)	<- c(names(y1), names(y2[[x]]))
    } else {
      x.out			<- cbind(y1[rep(x, nrow(y2)), ], y2)
      names(x.out)	<- c(names(y1), names(y2))
    }
    rownames(x.out)	<- 1:nrow(x.out)
    return(x.out)
  }
  
  if (is.null(table.vars)) {
    tables.columns	<- parseTIGetColumns(t.info.d.s, vars=column.vars, prefix=column.prefix)
  } else if (is.null(column.vars)) {
    tables.columns	<- list(parseTIGetTables(t.info.d.s, vars=table.vars, prefix=table.prefix))
  } else {
    tables			<- parseTIGetTables(t.info.d.s, vars=table.vars, prefix=table.prefix)
    columns			<- parseTIGetColumns(t.info.d.s, vars=column.vars, prefix=column.prefix)
    tables.columns	<- llply(1:nrow(tables), expandInfo, y1=tables, y2=columns)
  }
  
  if (rbind.results) {
    if (length(tables.columns) > 1) {
      tables.columns		<- na.omit(do.call('rbind', tables.columns))
    } else
      tables.columns		<- tables.columns[[1]]
  }
  
  return(tables.columns)
}

#================================================#
# parseTIGetTables
parseTIGetTables <- function(t.info, vars=c('id', 'name', 'menuPath', 'datasource', 'tableName', 'schemaName',
                                            'tableTypeId', 'showInUI', 'description', 'datePartitioned', 'segmentable',
                                            'streamingDataCollection', 'globallyPublished'),
                             prefix="table") {
  #----#
  #	Get table information for all tables in data suite.
  # Function used in getThenParseTableInfoXML.
  # Parameters
  #	    t.info	output from 'getFullTableListXML' function, i.e. table information for one data suite
  #	    vars	possible variables containing table information
  #	    prefix	text string added to front of each variable name to prevent possible duplications with variables at other levels
  # Returns
  #    data.frame object with table information for tables in data suite
  
  vars			<- match.arg(vars, several.ok=TRUE)
  if (is.null(prefix))
    var.names		<- vars
  else
    var.names		<- paste(prefix, vars, sep=".")
  
  len				<- length(t.info[[1]])
  if (len > 0) {
    tables			<- data.frame(ldply(t.info[[1]][1:len], function(x) t(xmlAttrs(x)[vars]))[, vars, drop=FALSE])
  } else {
    tables			<- data.frame(matrix(NA, nrow=1, ncol=length(var.names)))
  }
  names(tables)	<- var.names
  
  return(tables)
}

#================================================#
# parseTIGetColumns
parseTIGetColumns <- function(t.info, vars=c('id', 'tableColumnTypeId', 'columnName', 'name', 'description',
                                             'summable', 'required', 'showInUI', 'unique', 'presentationDefault',
                                             'aggregationTypeId', 'presentationTypeId', 'decimalPlaces'),
                              prefix=NULL) {
  
  #----#
  # Description
  #	Get column information for all tables in data suite.
  # Function used in getThenParseTableInfoXML.
  # Parameters
  #	    t.info	output from 'getFullTableListXML' function, i.e. table information for one data suite
  #		  vars	possible variables containing column information
  #	    prefix	text string added to front of each variable name to prevent possible duplications with variables at other levels
  # Returns
  #     list of data.frame objects with column information for all tables in data suite
  
  vars				<- match.arg(vars, several.ok=TRUE)
  if (is.null(prefix))
    var.names			<- vars
  else
    var.names			<- paste(prefix, vars, sep=".")
  
  num.columns			<- laply(t.info[[1]][1:length(t.info[[1]])], function(x) length(x[[1]]))
  num.tables			<- length(num.columns)
  
  if ((length(num.columns) > 1 | num.columns[1] > 0) & num.tables > 0) {
    col.info			<- as.list(1:num.tables)
    for (i in 1:num.tables) {
      len					<- num.columns[i]
      #			col.info[[i]]		<- data.frame(ldply(t.info[[1]][[i]][[1]][1:len], function(x) t(xmlAttrs(x)[vars]))[,-1])
      col.info[[i]]		<- data.frame(ldply(t.info[[1]][[i]][[1]][1:len], function(x) t(xmlAttrs(x)))[,vars])
      names(col.info[[i]])	<- var.names
    }
  } else {
    col.info			<- data.frame(matrix(NA, nrow=1, ncol=length(var.names)))
    names(col.info)		<- var.names
  }
  
  return(col.info)
}


#================================================#
# parseCompactUserInfo   
parseCompactUserInfo <- function(cui, acct.vars=c('id', 'nid', 'name', 'description', 'language', 'skin', 'admin'),
                                 acct.prefix="acct",
                                 d.s.vars=c('id', 'nid', 'name', 'description', 'timezone', 'timezoneOffset', 'admin'),
                                 d.s.prefix="d.s", var.maps=FALSE, include.accts=TRUE, rbind.results=TRUE) {
  # Put information from 'getCompactUserInfoXML' function into a list of data frames or a single data.frame
  # Params
  #    cui output from 'getCompactUserInfoXML' function
  #    acct.vars possible variables containing account information
  #    acct.prefix text string to add to the front of each account variable in order to prevent possible duplications
  #    d.s.vars possible variables containing data suite information
  #    d.s.prefix text string to add to the front of each data suite variable in order to prevent possible duplications
  #    var.maps if TRUE, then only those accounts and data suites containing variable replacement names will appear, otherwise all account & data.suite combinations are returned (default is FALSE)
  #    include.accts if TRUE, then account information is included with the data suite and variable information (default is TRUE)
  #    rbind.results if TRUE, a data.frame of results is returned, otherwise a list is returned (default is TRUE)
  # Returns
  #    data.frame
  
  
  accounts			<- parseCUIForAccts(cui=cui, vars=acct.vars, prefix=acct.prefix)
  num.accts			<- nrow(accounts)
  data.suites			<- as.list(1:num.accts)
  ui.table.maps		<- as.list(1:num.accts)
  var.mappings		<- as.list(1:num.accts)
  
  for (i in 1:num.accts) {
    data.suites[[i]]	<- parseCUIForDataSuites(acct=cui[[1]][[1]][[i]], vars=d.s.vars, prefix=d.s.prefix)
    
    if (var.maps) {
      ui.table.maps[[i]]	<- parseCUIForUiTableMaps(acct=cui[[1]][[1]][[i]])
      
      if (any(ui.table.maps[[i]] != 0)) {
        maps					<- ui.table.maps[[i]]
        suites					<- which(maps > 0)
        var.mappings[[i]]		<- parseCUIForVarNames(acct=cui[[1]][[1]][[i]], maps=ui.table.maps[[i]])
        
        for (j in 1:length(suites)) {
          
          if (include.accts) {
            var.mappings[[i]][[j]]	<- cbind(accounts[rep(i, nrow(var.mappings[[i]][[j]])),],
                                            data.suites[[i]][rep(suites[j],
                                                                 nrow(var.mappings[[i]][[j]])),], var.mappings[[i]][[j]])
            
            if (ncol(accounts) == 1 | ncol(data.suites[[i]]) == 1)
              names(var.mappings[[i]][[j]])[1:(ncol(accounts) + ncol(data.suites[[i]]))]	<-
                c(names(accounts), names(data.suites[[i]]))
            
          } else {
            var.mappings[[i]][[j]]	<- cbind(data.suites[[i]][rep(suites[j],
                                                                 nrow(var.mappings[[i]][[j]])),], var.mappings[[i]][[j]])
            
            if (ncol(data.suites[[i]]) == 1)
              names(var.mappings[[i]][[j]])[1]	<- names(data.suites[[i]])
          }
        }
        
        var.mappings[[i]]		<- do.call('rbind', var.mappings[[i]])
        rownames(var.mappings[[i]])		<- 1:nrow(var.mappings[[i]])
      } else
        var.mappings[[i]]		<- NA
    } else {
      var.mappings[[i]]	<- cbind(accounts[rep(i, nrow(data.suites[[i]])),], data.suites[[i]])
      dimnames(var.mappings[[i]])		<- list(1:nrow(var.mappings[[i]]), c(names(accounts),
                                                                        names(data.suites[[i]])))
    }
  }
  
  if (is.null(acct.prefix))
    var.names		<- acct.vars
  else
    var.names		<- paste(acct.prefix, acct.vars, sep=".")
  
  if (rbind.results) {
    var.mappings		<- na.omit(do.call('rbind', var.mappings))
    rownames(var.mappings)	<- 1:nrow(var.mappings)
  } else {
    if (length(position <- grep("[.]name", names(accounts))) > 0) {
      names(var.mappings)	<- accounts[, position]
    } else if (length(position <- grep("[.]id", names(accounts))) > 0) {
      names(var.mappings)	<- accounts[, position]
    } else {
      names(var.mappings)	<- paste("account", 1:length(var.mappings), sep=".")
    }
  }
  return(var.mappings)
}

#================================================#
# parseCUIForVarNames   
parseCUIForVarNames <- function(acct, maps=NULL) {
  #	Get replacement name information for all variables in an account
  # Arguments
  #	acct	one account's worth of output from 'getCompactUserInfoXML' function, i.e. cui[[1]][[1]][[k]] for kth account information
  #	maps	if NULL, function searches for "ui_table_mappings" objects in table information, otherwise requires output from 'parseCUIForUiTableMaps' function (default is NULL)
  # Value
  #	Returns data.frame object with column replacement name information for only those variables with replacements but for the entire account
  
  suite.pos		<- which(names(acct) == "reportSuites")
  if (length(suite.pos) > 0) {
    report.suites	<- acct[[suite.pos]]
    num.suites		<- length(report.suites)
    if (is.null(maps))
      maps			<- laply(report.suites[1:num.suites], function(x) { y <- names(x[[1]]);
      if (length(y) > 0) {
        if (is.element("ui_table_mappings", names(x[[1]])))
          which(names(x[[1]]) == "ui_table_mappings") else 0 } else 0  } )
  } else
    return("no reportSuites")
  
  suites			<- which(maps > 0)
  if (length(suites) > 0) {
    var.maps		<- as.list(1:length(suites))
    
    for (i in 1:length(suites)) {
      len				<- length(report.suites[[suites[i]]][[1]][[maps[suites[i]]]])
      var.maps[[i]]	<- ldply(report.suites[[suites[i]]][[1]][[maps[suites[i]]]][1:len], xmlAttrs)[,-1]
    }
    
  } else
    return("no ui_table_mappings")
  
  return(var.maps)
}

#================================================#
# parseCUIForUiTableMaps  
parseCUIForUiTableMaps <- function(acct) {
  # Find which data suites in an account have replacement names, i.e. have a "ui_table_mappings" section
  # Params
  #    acct account information for a single account from 'getCompactUserInfoXML' function
  # Return a numeric vector that indicates which data suites have variables with replacement names and the position within the data suite information providing the replacement names
  
  suite.pos		<- which(names(acct) == "reportSuites")
  if (length(suite.pos) > 0) {
    report.suites	<- acct[[suite.pos]]
    num.suites		<- length(report.suites)
    return(laply(report.suites[1:num.suites], function(x) { y <- names(x[[1]]); if (length(y) > 0) {
      if (is.element("ui_table_mappings", names(x[[1]])))
        which(names(x[[1]]) == "ui_table_mappings") else 0 } else 0  } ))
  } else
    return(0)
}

#================================================#
# parseSegmentInfoByDS
parseSegmentInfoByDS <- function(s.info, vars=c('id', 'nid', 'segmentTypeId', 'reportSuiteId', 'name', 'description', 'menuName', 'scriptBody'), prefix=NULL) {
  
  vars				<- match.arg(vars, several.ok=TRUE)
  if (is.null(prefix))
    var.names			<- vars
  else
    var.names			<- paste(prefix, vars, sep=".")
  
  num.segments		<- length(s.info[[1]])
  if (num.segments > 0) {
    if (num.segments == 1) {
      segments			<- data.frame(t(xmlAttrs(s.info[[1]][[1]])))
    } else {
      segments			<- ldply(1:num.segments, function(x) t(xmlAttrs(s.info[[1]][[x]])))
    }
    segments			<- segments[, intersect(var.names, names(segments))]
  } else {
    segments			<- data.frame(matrix(NA, nrow=1, ncol=length(var.names)))
    names(segments)		<- var.names
  }
  
  return(segments)
}


