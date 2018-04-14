library(httr)
library(jsonlite)

fake_people <- function(num_results = 20, 
                        gender_wanted = 'female',
                        nat = 'US') {
# Function will still need some error checking
# e.g. a country that's not valid should be removed
if(num_results > 1000) {
	stop("What the hell are you thinking?")
}

url <- "http://api.randomuser.me/"
args <- list(gender = gender_wanted,
             results = num_results,
             nat = nat)

query <- GET(url, query = args)
data <- content(query, "text")
results <- fromJSON(data, flatten = TRUE)
results
}
