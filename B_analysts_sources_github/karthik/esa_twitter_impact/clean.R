# Script to load and clean up the twitter data.
# load.R
# Karthik's first pass at the data. All tweets from Topsy are in the data/all_tweets.csv file.
# __________________________________________________________________________

tweets <- read.csv('~/Dropbox/esa_twitter_impact/data/all_tweets.csv', header=T)
dim(tweets)
# We've got 6777 records and 28 fields.
 names(tweets)
# "user.screen_name" - actual twitter handle
# "user.name" - Full name entered on profile
# "user.id_str" id number
# "user.created_at" date of account creation        
# "user.description" profile description
# "user.location" self-reported location
# "user.url" user added personal homepage
# "user.followers_count"         
# "user.friends_count" (not sure what this means)
# "user.statuses_count"          
# "user.lang" mostly en
# "user.time_zone"              
# "user.geo_enabled" location enabled tweets
# "user.verified" 3 celebrities here (Carl Zimmer, USFWSHQ, M. Sanjayan)               
# "topsy.influence"- no idea
# "text" - the actual tweet
# "id_str" - not sure
# "created_at" - time of tweet            
# "retweet_count" - number of retweets
# "in_reply_to_screen_name" - response to       
# "in_reply_to_status_id_str" - responding to which tweet
# "source" - tweeting from what client
# "coordinates.type" - location info
# "coordinates.coordinates.0"  - location info   
# "coordinates.coordinates.1" - location info
# "topsy.location.search_id" - topsy's attempt at locating the user.   
# "topsy.document_info.lang" - mostly en.
# "topsy.document_info.sentiment"- positive or negative tone of a tweet.
# __________________________________________________________________________



names(tweets) <- c("screen_name", "name", "id", "created_at", "desc", "location", "url", "followers", "friends", "status_count", "lang", "time_zone", "geo", "verified", "influence", "text", "tweet_id", "tweet_date", "retweets", "in_reply_to", "reply_id", "source", "coords_type", "cooords_lat", "coords_long", "location_search_id", "doc_lang", "sentiment")

tweets$tweet_date <- strptime(tweets$created_at,"%a %b %d %H:%M:%S %z %Y", tz="America/Los_Angeles")
tweets <- tweets[, -4]

# Now writing the cleaned file to disk.
write.csv(tweets, file="~/Dropbox/esa_twitter_impact/data/cleaned_twitter_data.csv")