library(RCurl)
library(RJSONIO)

searchTwitter =
function(curl, q, since_id = integer(), max_id = "",
         u = "https://api.twitter.com/1.1/search/tweets.json",
         ..., followNext = TRUE, count = 100L,
         args = list(q = q, count = count, result_type = "recent", ...))
{
    if(length(since_id) && ((is.numeric(since_id) && since_id > 0) || (is.character(since_id) && since_id != ""))) {
        cat("starting at tweet id", since_id, "\n")
        args$since_id = as.character(since_id)
    }

    orig.args = args
    
    if(length(max_id) && max_id != "")
        args$max_id = as.character(max_id)
    

    ans = tryCatch(getForm(u, .params = args, curl = curl),
              Service_Unavailable = function(e) {
                    Sys.sleep(20)
                    getForm(u, .params = args, curl = curl)       
              },
              GenericHTTPError = function(e) {
                     # need to catch rate limiting here.
                  if(e$message %in% c("Too Many Requests\r\n")) {
                      if(e$message == "Too Many Requests\r\n") {
                         cat("Rate limited\n")
                         print(e)
                           # x-rate-limit-reset tells us when the window ends. So we sleep until then.
                          dur = as.numeric(e$httpHeader["x-rate-limit-reset"]) - as.numeric(Sys.time()) + 1
                          cat("Sleeping for", dur, "seconds\n")
                      } else
                          dur = 30
                      Sys.sleep(dur)
                      getForm(u, .params = args, curl = curl)       
                  } else
                      stop(e)
              })
    rc = fromJSON(ans)


if(!("statuses" %in% names(rc)))  browser()
    hasMore = length(rc$statuses) == count && (rc$search_metadata$count == 100) && ("next_results" %in% names(rc$search_metadata)
                                                                              && rc$search_metadata$next_results != "")


    if(!followNext || !hasMore)
       return(rc)

    values = list()
    tmp = rc
    ctr = 1L
    while(hasMore) {
       orig.args["max_id"] = getFormParams(tmp$search_metadata$next_results)["max_id"]
 cat(ctr, length(values), "next id", orig.args[["max_id"]], "\n")
       tmp = searchTwitter(curl, args = orig.args, followNext = FALSE)

         # Check for errors, etc.
       values[[ length(values) + 1 ]] = tmp
       hasMore = (tmp$search_metadata$count == 100)  && "next_results" %in% names(tmp$search_metadata)   && tmp$search_metadata$next_results != ""
       ctr = ctr + 1L
   }
    
   invisible(values)
}


getRateLimit =
function(curl, resources = "search")
{    
  zz = getForm("https://api.twitter.com/1.1/application/rate_limit_status.json", resources = resources, curl = curl)
  fromJSON(zz)
}
