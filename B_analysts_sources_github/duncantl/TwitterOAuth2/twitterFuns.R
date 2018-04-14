library(RCurl)
library(ROAuth)
library(RJSONIO)

searchTwitter =
function(q, since_id = integer(), max_id = "",
         key = twitterKey, secret = twitterSecret, token = twitterToken, tokenSecret = twitterTokenSecret,
         u = "https://api.twitter.com/1.1/search/tweets.json",
        ..., followNext = TRUE,
        args = list(q = q, count = 100L, result_type = "recent", ...))
{
    if(length(since_id) && ((is.numeric(since_id) && since_id > 0) || (is.character(since_id) && since_id != ""))) {
        cat("starting at tweet id", since_id, "\n")
        args$since_id = as.character(since_id)
    }

    orig.args = args
    
    if(length(max_id) && max_id != "")
        args$max_id = as.character(max_id)
    
    args = ROAuth:::signRequest(u, args, key, secret, token, tokenSecret)

    tryCatch(ans <- getForm(u, .params = args),
              GenericHTTPError = function(e) {
                     # need to catch rate limiting here.
                  if(e$message == "Too Many Requests\r\n") {
       browser()
                           # x-rate-limit-reset tells us when the window ends. So we sleep until then.
                      dur = as.numeric(e$httpHeader["x-rate-limit-reset"]) - as.numeric(Sys.time()) + 1
                      Sys.sleep(dur)
                           # Now reissue the request.
                           # Can't reuse the same request as it has a nonce and also a timestamp.
                           # So Remove the oauth elements in args and resign.
                      args = args[ !grepl("^oauth", names(args)) ]
                      args = ROAuth:::signRequest(u, args, key, secret, token, tokenSecret)
                      getForm(u, .params = args)       
                  } else
                      stop(e)
              })
    rc = fromJSON(ans)
    
if(!("statuses" %in% names(rc)))  browser()
    hasMore = (rc$search_metadata$count == 100) && ("next_results" %in% names(rc$search_metadata)
                                  && rc$search_metadata$next_results != "")


    if(!followNext || !hasMore)
       return(rc)

    values = list()
    tmp = rc
    ctr = 1L
    while(hasMore) {
       orig.args["max_id"] = getFormParams(tmp$search_metadata$next_results)["max_id"]
 cat(ctr, length(values), "next id", orig.args[["max_id"]], "\n")
       tmp = searchTwitter(args = orig.args, key = key, secret = secret, token = token, tokenSecret = tokenSecret, followNext = FALSE)

         # Check for errors, etc.
       values[[ length(values) + 1 ]] = tmp
       hasMore = (tmp$search_metadata$count == 100)  && "next_results" %in% names(tmp$search_metadata)   && tmp$search_metadata$next_results != ""
       ctr = ctr + 1L
   }
    
   invisible(values)
}
