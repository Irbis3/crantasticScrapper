
library(RODBC)
library(babynames)
library(rbenchmark)

conn <- odbcDriverConnect("DSN=mysql")


babynames_1m <- as.data.frame(babynames)[1:1e6,]
babynames_10k <- as.data.frame(babynames)[1:1e4,]
babynames_1 <- babynames_1m[1,]


benchmark(
  {sqlDrop(conn, "babynames_10k"); sqlSave(conn, babynames_10k, "babynames_10k", rownames = FALSE)},
  {sqlDrop(conn, "babynames_10k"); sqlSave(conn, babynames_10k, "babynames_10k", rownames = FALSE)},
  {sqlDrop(conn, "babynames_10k"); sqlSave(conn, babynames_10k, "babynames_10k", rownames = FALSE)},
replications = 1, columns = c("test", "elapsed"), order = NULL)
#  elapsed
#1   8.284
#2   8.063
#3   8.176

#| babynames_10k | CREATE TABLE `babynames_10k` (
#  `year` double DEFAULT NULL,
#  `sex` varchar(255) DEFAULT NULL,
#  `name` varchar(255) DEFAULT NULL,
#  `n` int(11) DEFAULT NULL,
#  `prop` double DEFAULT NULL
#) ENGINE=InnoDB DEFAULT CHARSET=latin1 |

benchmark(
  babynames_1m_fromdb <- sqlQuery(conn, "select * from babynames_1m"),
  babynames_1m_fromdb <- sqlQuery(conn, "select * from babynames_1m"),
  babynames_1m_fromdb <- sqlQuery(conn, "select * from babynames_1m"),
replications = 1, columns = c("test", "elapsed"), order = NULL)
#                                                                 test elapsed
#1 babynames_1m_fromdb <- sqlQuery(conn, "select * from babynames_1m")   2.700
#2 babynames_1m_fromdb <- sqlQuery(conn, "select * from babynames_1m")   2.740
#3 babynames_1m_fromdb <- sqlQuery(conn, "select * from babynames_1m")   2.705

all.equal(babynames_1m, babynames_1m_fromdb)
#[1] "Component “sex”: Modes: character, numeric"                       
#[2] "Component “sex”: Attributes: < target is NULL, current is list >" 
#[3] "Component “sex”: target is character, current is factor"          
#[4] "Component “name”: Modes: character, numeric"                      
#[5] "Component “name”: Attributes: < target is NULL, current is list >"
#[6] "Component “name”: target is character, current is factor"  
sapply(babynames_1m, class)
#       year         sex        name           n        prop 
#  "numeric" "character" "character"   "integer"   "numeric" 
sapply(babynames_1m_fromdb, class)
#     year       sex      name         n      prop 
#"numeric"  "factor"  "factor" "integer" "numeric" 


benchmark(
   {sqlDrop(conn, "babynames_1"); sqlSave(conn, babynames_1, "babynames_1", rownames = FALSE)},
   {sqlDrop(conn, "babynames_1"); sqlSave(conn, babynames_1, "babynames_1", rownames = FALSE)},
   {sqlDrop(conn, "babynames_1"); sqlSave(conn, babynames_1, "babynames_1", rownames = FALSE)},
replications = 1000, columns = c("test", "elapsed"))
#  elapsed
#1  12.868
#2  13.166
#3  12.929

benchmark(
   babynames_1_fromdb <- sqlQuery(conn, "select * from babynames_1"),
replications = 10000, columns = c("test", "elapsed"))
#                                                               test elapsed
#1 babynames_1_fromdb <- sqlQuery(conn, "select * from babynames_1")   5.075


benchmark(
   sqlSave(conn, babynames_1, "babynames_1", rownames = FALSE, append = TRUE),
replications = 1000, columns = c("test", "elapsed"))
#  elapsed
#1   6.242



