library(httr)
library(ores)

# Handles the actual querying logic
query_wp <- function(params, error_message){
  result <- httr::GET("https://en.wikipedia.org/w/api.php",
                      query = params,
                      httr::user_agent("Dalit Dashboard service"))
  if(result$status_code != 200){
    stop(error_message)
  }
  return(httr::content(result))
}

