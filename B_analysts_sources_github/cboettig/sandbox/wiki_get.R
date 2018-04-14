#' Place a GET call to the mediawiki API using query action
#' @param titles the tile of the page to be grabbed. 
#' @param baseurl the base url of the Wiki, e.g. "http://en.wikipedia.org/w".  
#' @param format the desired format for the output
#' @param prop the revision desired.  Default is most recent first
#' @param rvprop What should be returned from the revision. Defaults to "content". see details. Can include more than one of the options. 
#' @returns the contents of a mediawiki page in the requested format
#' @import httr gsubfn 
#' @details
#' http://en.wikipedia.org/w/api.php?format=xml&action=query&titles=Main%20Page&prop=revisions&rvprop=content
#'Parameters:
#'  rvprop              - Which properties to get for each revision:
#'                         ids            - The ID of the revision
#'                         flags          - Revision flags (minor)
#'                         timestamp      - The timestamp of the revision
#'                         user           - User that made the revision
#'                         userid         - User id of revision creator
#'                         size           - Length (bytes) of the revision
#'                         sha1           - SHA-1 (base 16) of the revision
#'                         comment        - Comment by the user for revision
#'                         parsedcomment  - Parsed comment by the user for the revision
#'                         content        - Text of the revision
#'                         tags           - Tags for the revision
#'                        Values (separate with '|'): ids, flags, timestamp, user, userid, size, sha1, comment, parsedcomment, content, tags, flagged
#'                        Default: ids|timestamp|flags|comment|user
#'
#' @references See http://www.mediawiki.org/wiki/API:Main_page for and introduction, and http://en.wikipedia.org/w/api.php for the full documentation of the MediaWiki API
#' @export
wiki_get <- function(titles,  baseurl="http://wikipedia.org", format=c("xml", "json"), prop=c("revisions"), rvprop=c("content", "tags"), ...){
  format <- match.arg(format)
#  if(!is.null(baseurl))
  if(length(rvprop) > 1)
    rvprop <- paste0(rvprop, collapse="|")
  action="query"
  addr <- paste(baseurl, "/api.php?format=", format, "&action=", action, "&titles=", titles, "&prop=", prop, "&rvprop=", rvprop, sep="")
  config <- c(add_headers("User-Agent" = "rwiki"), ...)
  out <- GET(addr, config=config)
}

# 
wiki_parse <- function(page, baseurl, format="json", ...){
  require(httr)
  action = "parse"
  addr <- paste(baseurl, "/api.php?format=", format, "&action=", action, "&page=", page, sep="")
  config <- c(add_headers("User-Agent" = "rwiki"), ...)
  out <- GET(addr, config=config)
  parsed_content(out)
}

