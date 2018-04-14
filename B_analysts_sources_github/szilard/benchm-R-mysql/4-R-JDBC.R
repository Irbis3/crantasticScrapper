
library(RJDBC)
library(babynames)
library(rbenchmark)


drv <- JDBC("com.mysql.jdbc.Driver", "mysql-connector-java-5.1.36-bin.jar")
    conn <- dbConnect(drv, "jdbc:mysql://localhost:3306/bm", "root", "")


babynames_1m <- as.data.frame(babynames)[1:1e6,]
babynames_10k <- as.data.frame(babynames)[1:1e4,]
babynames_1 <- babynames_1m[1,]


benchmark(
  dbWriteTable(conn, "babynames_10k", babynames_10k, overwrite = TRUE, row.names = FALSE),
  dbWriteTable(conn, "babynames_10k", babynames_10k, overwrite = TRUE, row.names = FALSE),
  dbWriteTable(conn, "babynames_10k", babynames_10k, overwrite = TRUE, row.names = FALSE),
replications = 1, columns = c("test", "elapsed"), order = NULL)
#  elapsed
#1   8.520
#2   8.622
#3   8.555

#| babynames_10k | CREATE TABLE `babynames_10k` (
#  `year` double DEFAULT NULL,
#  `sex` varchar(255) DEFAULT NULL,
#  `name` varchar(255) DEFAULT NULL,
#  `n` int(11) DEFAULT NULL,
#  `prop` double DEFAULT NULL
#) ENGINE=InnoDB DEFAULT CHARSET=latin1 |

benchmark(
  babynames_1m_fromdb <- dbGetQuery(conn, "select * from babynames_1m"),
  babynames_1m_fromdb <- dbGetQuery(conn, "select * from babynames_1m"),
  babynames_1m_fromdb <- dbGetQuery(conn, "select * from babynames_1m"),
replications = 1, columns = c("test", "elapsed"), order = NULL)
#1 babynames_1m_fromdb <- dbGetQuery(conn, "select * from babynames_1m")   3.333
#2 babynames_1m_fromdb <- dbGetQuery(conn, "select * from babynames_1m")   4.026
#3 babynames_1m_fromdb <- dbGetQuery(conn, "select * from babynames_1m")   3.430

all.equal(babynames_1m, babynames_1m_fromdb)
#[1] "Component “prop”: Mean relative difference: 9.626189e-07"


benchmark(
   dbWriteTable(conn, "babynames_1", babynames_1, overwrite = TRUE, row.names = FALSE),
replications = 1000, columns = c("test", "elapsed"))
#  elapsed
#1  12.493

benchmark(
   babynames_1_fromdb <- dbGetQuery(conn, "select * from babynames_1"),
replications = 1000, columns = c("test", "elapsed"))
#                                                                 test elapsed
#1 babynames_1_fromdb <- dbGetQuery(conn, "select * from babynames_1")   3.549

all.equal(babynames_1, babynames_1_fromdb)
#[1] TRUE

benchmark(
   dbWriteTable(conn, "babynames_1", babynames_1, append = TRUE, row.names = FALSE),
replications = 1000, columns = c("test", "elapsed"))
#  elapsed
#1   12.24

#  append = TRUE not working (bug?)
# select count(*) from babynames_1;
# 1

