library(ROAuth)
library(twitteR)

reqURL = "https://api.twitter.com/oauth/request_token"
accessURL = "http://api.twitter.com/oauth/access_token"
authURL = "http://api.twitter.com/oauth/authorize"
twitCred <- OAuthFactory$new(consumerKey=getOption("TwitterKey"),
                             consumerSecret=getOption("TwitterSecret"),
                             requestURL=reqURL,
                             accessURL=accessURL,
                             authURL=authURL)

twitCred$handshake() ## Authenticate in browser

registerTwitterOAuth(twitCred)


extractURLs <- function(text){
  indices <- grep(".*(http://\\S.*).*", text)
  # modify for multiple urls
  urls <- gsub(".*(http://\\S.*).*", "\\1 ", df$text[indices])
}




