source("~/DSI/Education/Workshops/WebScraping/twitterCreds.R")
library(RCurl)
library(RJSONIO)

# https://dev.twitter.com/oauth/application-only
u = "https://api.twitter.com/oauth2/token"

auth  = sprintf("%s:%s", twitterKey, twitterSecret)

ans = postForm(u, grant_type = "client_credentials", style = "POST",
               .opts = list('useragent' = 'R Twitter App',
                             httpheader = c(Authorization = sprintf("Basic %s", base64Encode(auth)))))

bearerToken = fromJSON(ans)["access_token"]

con = getCurlHandle(httpheader = c(Authorization = sprintf("Bearer %s", bearerToken)))

searchURL = "https://api.twitter.com/1.1/search/tweets.json"
tt = getForm(searchURL, q = "@RepMarthaRoby", curl = con)


zz = getForm("https://api.twitter.com/1.1/application/rate_limit_status.json", resources = "search", curl = con)
