
library(babynames)
library(readr)

babynames_1m <- as.data.frame(babynames)[1:1e6,]

write_csv(babynames_1m, "/tmp/babynames_1m.csv", col_names = FALSE)

