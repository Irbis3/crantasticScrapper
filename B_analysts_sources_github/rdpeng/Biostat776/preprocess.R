## Read in the data
alltwitter <- readLines("en_US.twitter.txt")
tweets <- sample(alltwitter, round(0.05 * length(alltwitter)))  ## 5% sample
tweets <- tolower(tweets)

## Tokenize
regex <- "[a-z0-9']+"
matches <- gregexpr(regex, tweets, perl = TRUE)
tweetwords <- regmatches(tweets, matches)

## Use 90% of vocabulary
allwords <- unlist(tweetwords)
probs <- table(allwords) / length(allwords)
probs <- sort(probs, decreasing = TRUE)
words <- names(probs)
csum <- cumsum(probs)
idx <- which(csum > 0.9)[1]
use <- seq(1, idx)
words <- words[use]

save(words, file = "words.rda")
