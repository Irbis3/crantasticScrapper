# Reading the survey data and matching info to the larger twitter dataset
# Karthik Ram, 10/19/12
# ------------------------------------------
library(stringr)
library(plyr)
setwd('~/Dropbox/esa_twitter_impact')
survey <- read.csv('data/today.csv') 
# remove blank columns. Also removed respondent ID and surveyer ID and other irrleevant stuff
survey <- survey[, -c(1:5,6:9)]
names(survey) <-c ("screen_name", "other_name", "gender", "position", "other", "role", "org", "org_other", "org_other2", "org_other3","other4", "attended", "location_notattend", "usefulness","location_", "twitter_usefulness", "networking", "twitter_uses", "twitter_goals", "twitte_goals2", "twitter_goals3", "twitter_goals4")

# This saves the cleaned up survey to disk
write.csv(survey, file="data/cleaned_survey.csv")

# Now let's load up all the tweets and merge the two datasets

full_data <- read.csv('data/cleaned_twitter_data.csv', header = T)

#  For now we don't need the full dataset, just the subset that we need to fill in
full_data_subset <- full_data[, c(2,3,6)]
# remove duplicates since we just need to match once
full_data_subset <- full_data_subset[!duplicated(full_data_subset), ]
survey_subset <- survey[, c(1,3,4)]
tweeters <- merge(full_data_subset, survey_subset, by="screen_name", all.x = TRUE)
tweeters$position <- str_trim(tweeters$position, side = "both")

missing_tweeters <- tweeters[which(nchar(tweeters$position)==2), ]
# -----------------------------------------------------------------
## Side note. Some people answered the survey and also had some conflicting information from Topsy
# You can see those folks here.
# 
foo <- ddply(missing_tweeters, .(screen_name), function(x) { if(nrow(x)>1) return(x)})
foo
# Silly people entering Leipzig once, then Germany next. I'll fix these manually later.


# -----------------------------------------------------------------
# split the missing tweeters
missing_tweeters$url <- paste0("http://twitter.com/",missing_tweeters$screen_name)
mt_karthik <- missing_tweeters[1:456, ]
mt_lortie <- missing_tweeters[457:nrow(missing_tweeters), ]

# You take your file, and I'll take mine. We should just fill in info and save back to same file.
write.csv(mt_karthik, file = "data/mt_karthik.csv")
write.csv(mt_lortie, file = "data/mt_lortie.csv")

