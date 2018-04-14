user <- "TWITTER_USER"
password <- "TWITTER_PASSWORD"
dbname <- "SQL_DATABASE_NAME"
userSQL <- "SQL_USERNAME"
passwordSQL <- "SQL_PASSOWRD" 

#==============================================================================
# Download tweets mentioning zombies
#==============================================================================

source("functions.R")

#==============================================================================
# Local download (restart every 30 minutes using cronjob)
#==============================================================================

filename <- tempfile()

tryCatch(getStream(string=c("obama", "romney"), filename=filename, time=3600, user=user,
	password=password),	error=function(e) e)

while (exists("error")){
	message("Error found! Restarting...")
tryCatch(getStream(string=c("obama", "romney"), filename=filename, time=3600, user=user,
	password=password),	error=function(e) e)	
}

#==============================================================================
# Parse JSON to DF
#==============================================================================

tweets <- JSONtoDF(filename)

# cleaning date format and text fields
tweets$created_at <- format.twitter.date(tweets$created_at)
tweets$location <- clean.unicode(tweets$location)

# divide by politician mentioned
obama.tweets <- tweets[grep(pattern="obama", tweets$text, ignore.case=TRUE),]
romney.tweets <- tweets[grep(pattern="romney", tweets$text, ignore.case=TRUE),]

#==============================================================================
# Upload to SQL database
#==============================================================================

library(RMySQL)
drv = dbDriver("MySQL")
conn <- dbConnect(drv, dbname=dbname, user=userSQL,
 					password=passwordSQL, host="localhost")

## First commit (RUN ONCE)
# dbWriteTable(conn, "obamatweets", obama.tweets)
# dbWriteTable(conn, "romneytweets", romney.tweets)
# dbDisconnect(conn)

dbWriteTable(conn, "romneytweets", romney.tweets, append=TRUE)
dbWriteTable(conn, "obamatweets", obama.tweets, append=TRUE)

dbDisconnect(conn)