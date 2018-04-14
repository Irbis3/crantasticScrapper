dbname <- "SQL_DATABASE_NAME"
userSQL <- "SQL_USERNAME"
passwordSQL <- "SQL_PASSOWRD" 
yahooID <- "YAHOO_GEO_API_ID"

#==============================================================================
# Create timeline with number of tweets mentioning each candidate
#==============================================================================

## opening connection to mySQL server

drv = dbDriver("MySQL")
library(XML)
conn <- dbConnect(drv, dbname=dbname, user=userSQL,
 					password=passwordSQL, host="localhost")

## queries to extract summary statistics

query_obama <- '
select 
   month(created_at) month_data , 
   day(created_at) day_data , 
   hour(created_at) hour_data, 
   minute(created_at) minute_data, 
   count(*) counts
from
    obamatweets
group by month_data, day_data, hour_data, minute_data
'

query_romney <- '
select 
   month(created_at) month_data , 
   day(created_at) day_data , 
   hour(created_at) hour_data, 
   minute(created_at) minute_data, 
   count(*) counts
from
    romneytweets
group by month_data, day_data, hour_data, minute_data
'

## downloading summary data
obama <- dbGetQuery(conn, query_obama)
romney <- dbGetQuery(conn, query_romney)

## cleaning and organizing data

obama <- cleanDF(obama, cand="Obama")
romney <- cleanDF(romney, cand="Romney")

# merging data into a single dataframe

dygraphs_o <- obama[,c("date", "tweets")]
names(dygraphs_o) <- c("date", "Obama")
dygraphs_r <- romney[,c("date", "tweets")]
names(dygraphs_r) <- c("date", "Romney")
dygraphs <- merge(dygraphs_o, dygraphs_r, all.x=TRUE)

# cleaning date variable for javascript
dygraphs$date <- gsub("-", "/", dygraphs$date)

# computing TPM
dygraphs$Obama <- dygraphs$Obama / 10
dygraphs$Romney <- dygraphs$Romney / 10

# saving dataframe
write.csv(dygraphs, 
	file="tweets_timeline.csv", 
	row.names=F, quote=F)





