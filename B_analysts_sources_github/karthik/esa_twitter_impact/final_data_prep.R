rm(list = ls())
library(plyr)
library(data.table)
# -----------------------------------
setwd('~/Dropbox/esa_twitter_impact')
f1 <- read.csv('data/mt_karthik.csv')
f2 <- read.csv('data/mt_lortie.csv')
f1 <- f1[, 3:8]
f2 <- f2[, 2:7]
handles <- rbind(f1, f2)
handles <- handles[-which(handles$position == "SPAM"), ]
handles[which(handles$position=="No_idea"),]$position <- "Undetermined"
handles[which(is.na(handles$position)),]$position <- "Undetermined"
# Removing SPAM. Leaving in folks we couldn't identify as undetermined

# This is the full set of information about the folks who tweeted. This needs to be merged back with the original dataset for further analysis. Before I do so, I need to remove the spammers.
all_data <- read.csv('data/all_tweets.csv', header = T)
names(all_data)[1] <- "screen_name"

all_data <- data.table(all_data, key = "screen_name")
handles <- data.table(handles, key = "screen_name")

final_dataset <- all_data[handles, roll = T]
save(final_dataset, file = "data/final_dataset.rda")
# Use this for all further plotting besides analytics (which came pre computed from Topsy).
#
# Also saving as csv for non-R use.
write.csv(final_dataset, file = "data/final_dataset.csv")

