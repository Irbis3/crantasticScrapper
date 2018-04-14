source("utils.R")

get_last_edit_ <- function(titles){
  
  # Construct query parameters
  query_params <- list(action = "query",
                       prop = "revisions",
                       titles = paste0(titles, collapse = "|"),
                       rvprop = "ids",
                       format = "json")
  
  # Make query and return results
  results <- query_wp(query_params,
                      "Revision IDs could not be retrieved")
  
  return(results$query$pages)
}

get_last_edit <- function(titles){
  
  if(length(titles) > 50){
    titles <- split(titles, ceiling(seq_along(titles)/50))
  } else {
    titles <- list(titles)
  }
  
  results <- unlist(
    lapply(titles, get_last_edit_),
    recursive = FALSE, use.names = FALSE
  )
  
  return(unlist(lapply(results, function(x){
    return(x$revisions[[1]]$revid)
  })))
}

get_quality <- function(titles = NULL, edits = NULL){
  
  # Grab revIDs
  if(is.null(edits)){
    rev_ids <- get_last_edit(titles)
  } else {
    rev_ids <- edits
  }
  
  if(length(rev_ids) > 50){
    rev_ids <- split(rev_ids, ceiling(seq_along(rev_ids)/50))
  } else {
    rev_ids <- list(rev_ids)
  }
  
  # Get scores
  scores <- do.call("rbind",
                    lapply(rev_ids,
                           function(x){
                             return(ores::check_quality("enwiki", x))
                           }
                    )
  )
  
  return(scores)
}