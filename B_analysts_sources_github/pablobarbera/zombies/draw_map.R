dbname <- "SQL_DATABASE_NAME"
userSQL <- "SQL_USERNAME"
passwordSQL <- "SQL_PASSOWRD" 
yahooID <- "YAHOO_GEO_API_ID"

#==============================================================================
# Locate country where tweets were sent and draw map
#==============================================================================

library(googleVis)
library(RMySQL)
source("functions.R")

#==============================================================================
# Loading tweets
#==============================================================================

drv = dbDriver("MySQL")
library(XML)
conn <- dbConnect(drv, dbname=dbname, user=userSQL,
 					password=passwordSQL, host="localhost")

tweets <- dbGetQuery(conn, 'select id, location from zombiestweets WHERE date(created_at)>=SYSDATE() - INTERVAL 1 DAY')

# 1) Deleting tweets without location
tweets <- tweets[!is.na(tweets$location) & tweets$location!="",]

# 2) Removing unicode characters
tweets$location <- clean.unicode(tweets$location)

# 3) Get known locations from matches file (speeds up process, see below)
tweets$country <- NA
matches <- read.csv("matches.csv", stringsAsFactors=F)
tweets$country[is.na(tweets$country)] <- sapply(
	tweets$location[is.na(tweets$country)], 
	getMatches, 
	matches=matches)

# 4) Extracting coordinates
tweets$country[is.na(tweets$country)] <- sapply(
	tweets$location[is.na(tweets$country)], 
	getCoordinates, 
	yahooID=yahooID)

# 5) Extracting country names
tweets$country[is.na(tweets$country)] <- unlist(lapply(
	tweets$location[is.na(tweets$country)], 
	getCountry, 
	yahooID=yahooID))

# Saving known matches

# First commit of matches.csv (RUN ONCE)
# tweets <- getLocation(tweets, yahooID=yahooID)
# matches <- tweets[tweets$geoerror=="0",c("location", "country", "geoerror")]
# write.csv(matches, file="matches.csv", row.names=FALSE)

# Saving new matches
newmatches <- tweets[(tweets$location %in% matches$location)==FALSE, c("location", "country")]
newmatches <- newmatches[!duplicated(newmatches$location),]
write.table(newmatches, file="matches.csv", append=TRUE, sep=",", row.names=FALSE, col.names=FALSE)

# Aggregating by country
tweets$freq <- 1
zombietweets <- aggregate(tweets$freq, by=list(country=tweets$country), FUN=sum)
names(zombietweets)[2] <- "Tweets mentioning zombies in the past 24 hours"

#==============================================================================
# Drawing map
#==============================================================================

geo <- gvisGeoMap(zombietweets, locationvar="country", numvar="Tweets mentioning zombies in the past 24 hours",
                       options=list(dataMode='regions', width='1200px', height='600px'))

# print into html, with a few changes
geo$html$header <- gsub("<title>(.*)</title>", "<title>Zombie Outbreak Detector</title>", geo$html$header)
geo$html$caption <- paste("<div><span>Data: number of mentions to zombie events, by country, extracted using the Twitter ",
	"Streaming API &#8226; by <a href=\"http://www.pablobarbera.com\">Pablo Barberá</a></span><br />", sep="")
geo$html$footer <- paste("\n<!-- htmlFooter -->\n<span> \n Built with R and <a href=\"http://code.google.com/p/google-motion-charts-with-r/",
				"\">googleVis</a>\n&#8226; <a href=\"https://developers.google.com/terms/\">Google Terms of Use</a> &#8226; ",
				"<a href=\"https://google-developers.appspot.com/chart/interactive/docs/gallery/geomap.html#Data_Policy\">",
				"Data Policy</a>\n &#8226;  Source code available in <a href=\"https://github.com/pablobarbera/zombies\">Github</a> &#8226; ",
				"Last update: ", Sys.time(), "</span><br />",
				"</span></div>\n</body>\n</html>\n", sep="")
geo$html$chart <- gsub("Tweets.mentioning.zombies.in.the.past.24.hours", "Tweets mentioning zombies in the past 24 hours",
	x=geo$html$chart)
html <- unlist(geo$html, recursive=TRUE, use.names=FALSE)
writeLines(html, con="index.html")
