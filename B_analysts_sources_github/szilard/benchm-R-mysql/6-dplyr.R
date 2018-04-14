
library(dplyr)
library(babynames)
library(rbenchmark)


babynames_1m <- as.data.frame(babynames)[1:1e6,]
babynames_10k <- as.data.frame(babynames)[1:1e4,]
babynames_1 <- babynames_1m[1,]

src <- src_mysql("bm", host = "localhost", user = "root", password = "")
d_1m <- tbl(src, "babynames_1m")
d_1 <- tbl(src, "babynames_1")


benchmark(
  {db_drop_table(src$con, "babynames_1m"); copy_to(src, babynames_1m)},
  {db_drop_table(src$con, "babynames_1m"); copy_to(src, babynames_1m)},
  {db_drop_table(src$con, "babynames_1m"); copy_to(src, babynames_1m)},
replications = 1, columns = c("test", "elapsed"), order = NULL)
#  elapsed
#1  10.411
#2  10.330
#3  10.302

benchmark(
  df_1m <- collect(d_1m),
  df_1m <- collect(d_1m),
  df_1m <- collect(d_1m),
replications = 1, columns = c("test", "elapsed"), order = NULL)
#                    test elapsed
#1 df_1m <- collect(d_1m)   2.221
#2 df_1m <- collect(d_1m)   2.212
#3 df_1m <- collect(d_1m)   2.209

all.equal(babynames_1m, df_1m)
#[1] "Attributes: < Component “class”: Lengths (1, 3) differ (string compare on first 1) >"
#[2] "Attributes: < Component “class”: 1 string mismatch >" 
class(babynames_1m)
#[1] "data.frame"
class(df_1m)
#[1] "tbl_df"     "tbl"        "data.frame"


benchmark(
  {db_drop_table(src$con, "babynames_1"); copy_to(src, babynames_1)},
replications = 1000, columns = c("test", "elapsed"))
#  elapsed
#1   13.51

benchmark(
  df_1 <- collect(d_1),
replications = 10000, columns = c("test", "elapsed"))
#                  test elapsed
#1 df_1 <- collect(d_1)   6.156

all.equal(babynames_1, df_1)


