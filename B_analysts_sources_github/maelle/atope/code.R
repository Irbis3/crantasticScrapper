library("rtweet")
library("readr")
library("dplyr")
library("monkeylearn")

rladies_mad <- get_timeline(user = "RLadiesMAD",
                            n = 1000)

write_csv(rladies_mad, path = "data/timeline.csv")


# filter only tweets, no direct tweets or retweets
rladies_mad <- select(rladies_mad, - coordinates)
rladies_mad <- filter(rladies_mad, !is_retweet, is.na(in_reply_to_status_id))

# Spanish
rladies_mad <- filter(rladies_mad, lang == "es")

# sentimientos algo 1 "Sentiment Español"
sentimientos <- monkeylearn_classify(rladies_mad$text,
                                 classifier_id = "cl_TNtsmuoJ")
# the algo can give several label per tweet
sentimientos <- group_by(sentimientos, text_md5)
sentimientos <- summarize(sentimientos, label = toString(label))
sentimientos <- bind_cols(select(rladies_mad, text),
                         sentimientos)
write_csv(sentimientos, path = "output/algo1_output.csv")

# sentimientos algo 2 "Spanish Tweets Sentiment Analysis"
sentimientos <- monkeylearn_classify(rladies_mad$text,
                                     classifier_id = "cl_u9PRHNzf")
sentimientos <- bind_cols(select(rladies_mad, text),
                          sentimientos)
write_csv(sentimientos, path = "output/algo2_output.csv")