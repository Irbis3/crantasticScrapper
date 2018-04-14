
library(RMySQL)
library(babynames)
library(rbenchmark)

conn <- dbConnect(MySQL(), host = "localhost", user = "root", password = "", dbname = "bm")

babynames_1m <- as.data.frame(babynames)[1:1e6,]
babynames_10k <- as.data.frame(babynames)[1:1e4,]
babynames_1 <- babynames_1m[1,]


benchmark(
  dbWriteTable(conn, "babynames_10k", babynames_10k, overwrite = TRUE, row.names = FALSE),
  dbWriteTable(conn, "babynames_10k", babynames_10k, overwrite = TRUE, row.names = FALSE),
  dbWriteTable(conn, "babynames_10k", babynames_10k, overwrite = TRUE, row.names = FALSE),
replications = 1, columns = c("test", "elapsed"), order = NULL)
#  elapsed
#1   8.339
#2   8.544
#3   8.216


benchmark(
  babynames_1m_fromdb <- dbGetQuery(conn, "select * from babynames_1m"),
  babynames_1m_fromdb <- dbGetQuery(conn, "select * from babynames_1m"),
  babynames_1m_fromdb <- dbGetQuery(conn, "select * from babynames_1m"),
replications = 1, columns = c("test", "elapsed"), order = NULL)
#                                                                   test elapsed
#1 babynames_1m_fromdb <- dbGetQuery(conn, "select * from babynames_1m")   1.101
#2 babynames_1m_fromdb <- dbGetQuery(conn, "select * from babynames_1m")   1.096
#3 babynames_1m_fromdb <- dbGetQuery(conn, "select * from babynames_1m")   1.088

all.equal(babynames_1m, babynames_1m_fromdb)



benchmark(
   dbWriteTable(conn, "babynames_1", babynames_1, overwrite = TRUE, row.names = FALSE),
replications = 1000, columns = c("test", "elapsed"))
#  elapsed
#1    13.1

benchmark(
   babynames_1_fromdb <- dbGetQuery(conn, "select * from babynames_1"),
replications = 10000, columns = c("test", "elapsed"))
#                                                                 test elapsed
#1 babynames_1_fromdb <- dbGetQuery(conn, "select * from babynames_1")   4.164


benchmark(
   dbWriteTable(conn, "babynames_1", babynames_1, append = TRUE, row.names = FALSE),
replications = 1000, columns = c("test", "elapsed"))
#  elapsed
#1   4.066

