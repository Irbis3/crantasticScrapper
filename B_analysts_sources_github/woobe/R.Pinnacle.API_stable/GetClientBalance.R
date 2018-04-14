#' Get Client Balance
#'
#' @param force Default=TRUE , boolean if TRUE force a reload of the data if FALSE use cached data
#'
#' @return vector client balance parameter
#' @export
#'
#' @import httr
#' @import rjson
#' @examples
#' GetClientBalance()
GetClientBalance <- function(force=TRUE){
  CheckTermsAndConditions()
  
  if(length(.PinnacleAPI$ClientBalance)==0 || force){
    r <- GET(paste0(.PinnacleAPI$url ,"/v1/client/balance"),
             add_headers("Authorization"= authorization(),
                         "Content-Type" = "application/json")
    )
    .PinnacleAPI$ClientBalance <- fromJSON(content(r, "text"))
  }
  .PinnacleAPI$ClientBalance
}
