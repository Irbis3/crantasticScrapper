#' Get Leagues for Sport(s) by name
#'
#'  Returns all Leagues for the Sport(s) 
#'
#' @param sports  character vector of sports names.
#' @param force Default=FALSE, boolean if TRUE force a reload of the data if FALSE use cached data
#' @param regex Default=FALSE, boolean if TRUE , retreives sports id using regular expression on names
#'
#' @return a data frame having columns:
#' \itemize{
#' \item LeagueID
#' \item LinesAvailable
#' \item HomeTeam
#' \item AllowRoundRobin
#' \item LeagueName
#' }
#'@export
#'@examples
#'
#'GetLeagues("Soccer")
#'GetLeagues(c("Soccer","Boxing"))
#'GetLeagues(c("soccer","box"),regex=TRUE)


GetLeagues <-function(sports,force=FALSE,regex=FALSE){
  ## this is called once
  CheckTermsAndConditions()
  
  sports.all <- GetSports(force)
  ids <- sports.all[,"SportID"]
  ids.serach <- if(!regex) {
    ids[match(sports,sports.all[,"SportName"])]
  } else {
    patt <- paste(tolower(sports),collapse='|')
    ids[grepl(patt,tolower(sports.all[,"SportName"]))]
  }
  do.call(rbind,
          lapply(ids.serach,GetLeaguesByID))
  
}