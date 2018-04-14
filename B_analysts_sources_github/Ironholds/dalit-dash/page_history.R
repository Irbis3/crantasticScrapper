# Check a title existed at a specified date
check_existence <- function(titles, date){
  
  # For each title...
  timestamps <- lapply(titles, function(title){
    
    # Construct query parameters
    query_params <- list(action = "query",
                         prop = "revisions",
                         titles = title,
                         rvprop = "timestamp",
                         format = "json",
                         rvdir = "newer",
                         rvlimit = 1)

    # Make query and return results
    results <- query_wp(query_params,
                        "Revision IDs could not be retrieved")$query$pages[[1]]
    if("missing" %in% names(results)){
      return(NA)
    }
    return(results$revisions[[1]]$timestamp)
  })
  
  ts <- as.POSIXlt(gsub(x = unlist(timestamps), pattern = "T", replacement = " "), tz = "UTC")
  
  existed <- (!is.na(ts) & as.Date(ts) < date)
  
  return(titles[existed])
}

# For a set of pages, grab the last RevID prior to a specified date
get_last_revisions <- function(existing_pages, date){
  
  # Reformat the date
  date <- paste0(gsub(x = as.character(date), pattern = "-", replacement = ""),
                 "000000")
  
  revision_ids <- lapply(existing_pages, function(title, date){
    
    # Construct query parameters
    query_params <- list(action = "query",
                         prop = "revisions",
                         titles = title,
                         rvprop = "ids|timestamp",
                         format = "json",
                         rvstart = date,
                         rvlimit = 1,
                         rvdir = "older")
    
    # Make query and return results
    results <- query_wp(query_params,
                        "Revision IDs could not be retrieved")$query$pages[[1]]
    return(results$revisions[[1]]$revid)
  }, date = date)
  
  return(unlist(revision_ids))
}