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

tryCatch(getStream(string="zombi", filename=filename, time=1800, user=user,
	password=password),	error=function(e) e)

while (exists("error")){
	message("Error found! Restarting...")
tryCatch(getStream(string="zombi", filename=filename, time=1800, user=user,
	password=password),	error=function(e) e)	
}

#==============================================================================
# Parse JSON to DF
#==============================================================================

tweets <- JSONtoDF(filename)

# cleaning date format and text fields
tweets$created_at <- format.twitter.date(tweets$created_at)
tweets$location <- clean.unicode(tweets$location)

#==============================================================================
# Upload to SQL database
#==============================================================================

library(RMySQL)
drv = dbDriver("MySQL")
conn <- dbConnect(drv, dbname=dbname, user=userSQL,
 					password=passwordSQL, host="localhost")

## First commit (RUN ONCE)
# dbWriteTable(conn, "zombiesteweets", tweets)
# dbDisconnect(conn)


dbWriteTable(conn, "zombiestweets", tweets, append=TRUE)
dbDisconnect(conn)


