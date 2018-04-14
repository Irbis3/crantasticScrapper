#source("twitterCreds.R")
source("~/DSI/Education/Workshops/WebScraping/twitterCreds.R")
source("twitterFuns2.R")
source("utils.R")


if(!exists("bearerToken")) {
u = "https://api.twitter.com/oauth2/token"
auth  = sprintf("%s:%s", twitterKey, twitterSecret)
ans = postForm(u, grant_type = "client_credentials", style = "POST",
               .opts = list('useragent' = 'R Twitter App',
                             httpheader = c(Authorization = sprintf("Basic %s", base64Encode(auth)))))
bearerToken = fromJSON(ans)["access_token"]
}

con = getCurlHandle(httpheader = c(Authorization = sprintf("Bearer %s", bearerToken)))

 # read the logins
if(!exists("logins"))
  logins = readLines("Arkansas.csv")[1:20]  # just first 20 for now.

 # a numeric vector that will contain the since_id for
 # each login so that we can request only the tweets since that
 # previous tweet we collected in the previous iteration.

 # See https://dev.twitter.com/rest/public/timelines
if(!exists("since_ids"))
  since_ids = structure(numeric(length(logins)), names = logins)

# Rate limiting: 180 requests per 15 minutes, i.e. 900 seconds.
# So can only have a request every 5 seconds.

numIters = 1L
while(TRUE) {
    
 cur = Sys.time()
 for(id in logins) {

     o = searchTwitter(con, id, since_ids[id])
     cat(id, length(o), "\n")
     dir = file.path("Data", id)
     if(!(is.directory(dir))) {
        if(!dir.create(dir))
            stop("Problem creating directory ", dir)
     }

     count = length(list.files(dir)) + 1L
     filename = file.path("Data", id, paste0(count, ".rda"))
     save(o, file = filename)
     since_ids[id] = max(since_ids[id], getMaxID(o))
 }
 end = Sys.time()
 cat("Iteration", numIters, "took", end - cur, "\n")
 numIters = numIters + 1L
 save(since_ids, file = "since_ids.rda")
}

#searchTwitter('johnboehner', n=1000, since='2014-09-27', until='2014-09-29')
#userTimeline('johnboehner', n=100)
