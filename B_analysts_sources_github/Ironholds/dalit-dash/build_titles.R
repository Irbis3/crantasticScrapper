source("utils.R")

# Cleans up category names to ensure the Category:
# prefix is added, and any spaces are replaced with
# underscores.
clean_cat <- function(category){
  if(!grepl(x = category, pattern = "Category")){
    category <- paste0("Category:", category)
  }
  return(gsub(x = category, pattern = " ", replacement = "_"))
}

# Loop a function to allow for the continue tokens to be used
loop_func <- function(func, ...){
  
  output <- NULL
  
  holding <- func(...)
  
  while("continue" %in% names(holding)){
    output <- c(output, holding$query)
    holding <- func(..., continue_token = holding$continue$cmcontinue)
  }
  output <- c(output, holding$query)
  
  return(unlist(output, recursive = FALSE, use.names = FALSE))
}

# Get all subcategories within a category (as well as the
# category itself) so that we can easily generate a non-duplicated
# list of page titles.
get_categories <- function(category, continue_token = NULL){
  
  # Clean category name and construct query
  query_params <- list(action = "query",
                       list = "categorymembers",
                       format = "json",
                       cmtitle = clean_cat(category),
                       cmprop = "title",
                       cmtype = "subcat",
                       cmlimit = 50)
  
  # Handle continuation token
  if(!is.null(continue_token)){
    query_params['cmcontinue'] <- continue_token
  }
  
  # Make query and return results
  results <- query_wp(query_params,
                      "Categories could not be retrieved")
  
  return(results)
}

# Get all pages within a category, non-recursively, so that
# we can retrieve all the page titles
get_pages <- function(category, continue_token = NULL){
  
  # Construct parameters
  query_params <- list(action = "query",
                       list = "categorymembers",
                       format = "json",
                       cmtitle = clean_cat(category),
                       cmprop = "title",
                       cmtype = "page",
                       cmlimit = 50)
  
  # Handle continuation token
  if(!is.null(continue_token)){
    query_params['cmcontinue'] = continue_token
  }
  
  # Make query and return results
  results <- query_wp(query_params,
                      "Pages in categories could not be retrieved")
  
  return(results)
}

# Generate the page title list to use in other stages of the work.
get_titles <- function(category = "Dalit"){
  
  # Generate categories to retrieve
  categories <- unlist(
    lapply(loop_func(get_categories, category),
           function(x){
             return(x$title)
             }
           )
  )
  
  output <- lapply(categories, function(x){
    pages <- lapply(loop_func(get_pages, x),
                    function(y){
                      if(y$ns == 0){
                        return(y$title)
                      }
                    }
    )
    return(pages)
  })
  
  return(gsub(x = unique(unlist(output)), pattern = " ", replacement = "_"))
}