
library(RMySQL)
library(babynames)

conn <- dbConnect(MySQL(), host = "localhost", user = "root", password = "", dbname = "bm")

d_1m <- as.data.frame(babynames)[1:1e6,]
d_10m <- do.call("rbind", list(d_1m,d_1m,d_1m,d_1m,d_1m,d_1m,d_1m,d_1m,d_1m,d_1m))

gc()
dbWriteTable(conn, "d_10m", d_10m, overwrite = TRUE, row.names = FALSE)

gc()


###########################


library(RMySQL)

conn <- dbConnect(MySQL(), host = "localhost", user = "root", password = "", dbname = "bm")

d <-  dbGetQuery(conn, "select * from d_10m")

gc()

# while true; do ps -F $(pgrep R) | grep -v RSS; sleep 1; done | tee xx

#ubuntu    2351  1521  0 45679 34600   1 03:46 pts/1    S+     0:00 /usr/lib/R/bin/exec/R
#ubuntu    2351  1521  0 45679 34600   1 03:46 pts/1    S+     0:00 /usr/lib/R/bin/exec/R
#ubuntu    2351  1521  0 45679 34600   1 03:46 pts/1    S+     0:00 /usr/lib/R/bin/exec/R
#ubuntu    2351  1521  0 45679 34600   1 03:46 pts/1    S+     0:00 /usr/lib/R/bin/exec/R
#ubuntu    2351  1521  0 51707 58852   2 03:46 pts/1    R+     0:00 /usr/lib/R/bin/exec/R
#ubuntu    2351  1521  1 57742 82924   2 03:46 pts/1    S+     0:01 /usr/lib/R/bin/exec/R
#ubuntu    2351  1521  2 69973 131724  2 03:46 pts/1    R+     0:01 /usr/lib/R/bin/exec/R
#ubuntu    2351  1521  2 69973 131724  2 03:46 pts/1    S+     0:02 /usr/lib/R/bin/exec/R
#ubuntu    2351  1521  3 69973 131724  2 03:46 pts/1    R+     0:02 /usr/lib/R/bin/exec/R
#ubuntu    2351  1521  3 93973 227704  2 03:46 pts/1    R+     0:03 /usr/lib/R/bin/exec/R
#ubuntu    2351  1521  4 93973 227704  2 03:46 pts/1    S+     0:04 /usr/lib/R/bin/exec/R
#ubuntu    2351  1521  4 93973 227704  2 03:46 pts/1    S+     0:04 /usr/lib/R/bin/exec/R
#ubuntu    2351  1521  5 93973 227704  2 03:46 pts/1    R+     0:05 /usr/lib/R/bin/exec/R
#ubuntu    2351  1521  5 93973 227704  2 03:46 pts/1    S+     0:05 /usr/lib/R/bin/exec/R
#ubuntu    2351  1521  6 141973 419860 2 03:46 pts/1    S+     0:06 /usr/lib/R/bin/exec/R
#ubuntu    2351  1521  6 141973 419860 2 03:46 pts/1    R+     0:06 /usr/lib/R/bin/exec/R
#ubuntu    2351  1521  7 141973 419860 2 03:46 pts/1    R+     0:07 /usr/lib/R/bin/exec/R
#ubuntu    2351  1521  7 141973 419860 2 03:46 pts/1    S+     0:07 /usr/lib/R/bin/exec/R
#ubuntu    2351  1521  7 141973 419860 2 03:46 pts/1    S+     0:08 /usr/lib/R/bin/exec/R
#ubuntu    2351  1521  8 141973 419860 2 03:46 pts/1    S+     0:08 /usr/lib/R/bin/exec/R
#ubuntu    2351  1521  8 141973 419860 2 03:46 pts/1    S+     0:09 /usr/lib/R/bin/exec/R
#ubuntu    2351  1521  9 141973 419860 2 03:46 pts/1    R+     0:09 /usr/lib/R/bin/exec/R
#ubuntu    2351  1521  9 141973 419860 2 03:46 pts/1    R+     0:10 /usr/lib/R/bin/exec/R
#ubuntu    2351  1521  9 141973 419860 2 03:46 pts/1    S+     0:10 /usr/lib/R/bin/exec/R
#ubuntu    2351  1521 10 205972 600460 2 03:46 pts/1    R+     0:11 /usr/lib/R/bin/exec/R
#ubuntu    2351  1521 10 237973 803840 2 03:46 pts/1    R+     0:12 /usr/lib/R/bin/exec/R
#ubuntu    2351  1521 11 237973 803840 2 03:46 pts/1    R+     0:12 /usr/lib/R/bin/exec/R
#ubuntu    2351  1521 11 237973 803840 2 03:46 pts/1    R+     0:13 /usr/lib/R/bin/exec/R
#ubuntu    2351  1521 11 237973 803840 2 03:46 pts/1    S+     0:13 /usr/lib/R/bin/exec/R
#ubuntu    2351  1521 12 207628 682320 2 03:46 pts/1    S+     0:14 /usr/lib/R/bin/exec/R
#ubuntu    2351  1521 12 207628 682320 2 03:46 pts/1    S+     0:14 /usr/lib/R/bin/exec/R
#ubuntu    2351  1521 12 207628 682320 2 03:46 pts/1    S+     0:14 /usr/lib/R/bin/exec/R
#ubuntu    2351  1521 12 207628 682320 2 03:46 pts/1    S+     0:14 /usr/lib/R/bin/exec/R
#ubuntu    2351  1521 11 207628 682320 2 03:46 pts/1    S+     0:14 /usr/lib/R/bin/exec/R
#ubuntu    2351  1521 11 207628 682320 2 03:46 pts/1    S+     0:14 /usr/lib/R/bin/exec/R
#ubuntu    2351  1521 11 207628 682320 2 03:46 pts/1    S+     0:14 /usr/lib/R/bin/exec/R
#ubuntu    2351  1521 11 207628 682320 2 03:46 pts/1    S+     0:14 /usr/lib/R/bin/exec/R
#ubuntu    2351  1521 11 207628 682320 2 03:46 pts/1    S+     0:14 /usr/lib/R/bin/exec/R
#ubuntu    2351  1521 11 207628 682320 2 03:46 pts/1    S+     0:14 /usr/lib/R/bin/exec/R
#gc:
#ubuntu    2351  1521 11 143626 426576 0 03:46 pts/1    S+     0:14 /usr/lib/R/bin/exec/R
#ubuntu    2351  1521 11 143626 426576 0 03:46 pts/1    S+     0:14 /usr/lib/R/bin/exec/R
#ubuntu    2351  1521 11 143626 426576 0 03:46 pts/1    S+     0:14 /usr/lib/R/bin/exec/R
#ubuntu    2351  1521 11 143626 426576 0 03:46 pts/1    S+     0:14 /usr/lib/R/bin/exec/R

object.size(d)/1e3
402409.232 bytes

(803840-34600)/402409.232
1.911586

(426576-34600)/402409.232
0.9740731


###########################

options(java.parameters = "- Xmx3g")

library(RJDBC)

drv <- JDBC("com.mysql.jdbc.Driver", "mysql-connector-java-5.1.36-bin.jar")
conn <- dbConnect(drv, "jdbc:mysql://localhost:3306/bm", "root", "")


d <-  dbGetQuery(conn, "select * from d_10m")

gc()

#   Unable to retrieve JDBC result set for select * from d_10m (GC overhead limit exceeded)

#ubuntu    3339  1521  5 1356100 76984 2 04:04 pts/1    Sl+    0:00 /usr/lib/R/bin/exec/R
#ubuntu    3339  1521  5 1356100 76984 2 04:04 pts/1    Sl+    0:00 /usr/lib/R/bin/exec/R
#ubuntu    3339  1521  5 1356100 76984 2 04:04 pts/1    Sl+    0:00 /usr/lib/R/bin/exec/R
#ubuntu    3339  1521  6 1356617 134708 2 04:04 pts/1   Sl+    0:01 /usr/lib/R/bin/exec/R
#ubuntu    3339  1521  8 1356617 243856 2 04:04 pts/1   Rl+    0:01 /usr/lib/R/bin/exec/R
#ubuntu    3339  1521 16 1356617 316972 2 04:04 pts/1   Sl+    0:03 /usr/lib/R/bin/exec/R
#ubuntu    3339  1521 19 1356617 373396 3 04:04 pts/1   Sl+    0:04 /usr/lib/R/bin/exec/R
#ubuntu    3339  1521 26 1356617 486452 0 04:04 pts/1   Sl+    0:06 /usr/lib/R/bin/exec/R
#ubuntu    3339  1521 30 1356617 501892 0 04:04 pts/1   Sl+    0:07 /usr/lib/R/bin/exec/R
#ubuntu    3339  1521 31 1356617 634116 0 04:04 pts/1   Sl+    0:08 /usr/lib/R/bin/exec/R
#ubuntu    3339  1521 33 1356617 832108 1 04:04 pts/1   Rl+    0:08 /usr/lib/R/bin/exec/R
#ubuntu    3339  1521 32 1356617 832108 0 04:04 pts/1   Sl+    0:09 /usr/lib/R/bin/exec/R
#ubuntu    3339  1521 36 1356617 1069380 0 04:04 pts/1  Sl+    0:10 /usr/lib/R/bin/exec/R
#ubuntu    3339  1521 46 1356617 1153664 0 04:04 pts/1  Sl+    0:14 /usr/lib/R/bin/exec/R
#ubuntu    3339  1521 50 1356617 1156156 3 04:04 pts/1  Sl+    0:16 /usr/lib/R/bin/exec/R
#ubuntu    3339  1521 51 1356617 1160668 3 04:04 pts/1  Sl+    0:16 /usr/lib/R/bin/exec/R
#ubuntu    3339  1521 52 1356617 1353392 0 04:04 pts/1  Sl+    0:17 /usr/lib/R/bin/exec/R
#ubuntu    3339  1521 53 1356617 1387912 2 04:04 pts/1  Sl+    0:18 /usr/lib/R/bin/exec/R
#ubuntu    3339  1521 54 1356617 1558832 3 04:04 pts/1  Rl+    0:19 /usr/lib/R/bin/exec/R
#ubuntu    3339  1521 59 1356617 1749828 0 04:04 pts/1  Sl+    0:21 /usr/lib/R/bin/exec/R
#ubuntu    3339  1521 68 1356617 1764172 0 04:04 pts/1  Sl+    0:25 /usr/lib/R/bin/exec/R
#ubuntu    3339  1521 75 1356617 1802660 0 04:04 pts/1  Sl+    0:29 /usr/lib/R/bin/exec/R
#ubuntu    3339  1521 78 1356617 1888020 1 04:04 pts/1  Sl+    0:31 /usr/lib/R/bin/exec/R
#ubuntu    3339  1521 79 1356617 1891616 0 04:04 pts/1  Sl+    0:32 /usr/lib/R/bin/exec/R
#ubuntu    3339  1521 80 1356617 1943640 0 04:04 pts/1  Sl+    0:33 /usr/lib/R/bin/exec/R
#ubuntu    3339  1521 81 1356617 1966180 0 04:04 pts/1  Sl+    0:34 /usr/lib/R/bin/exec/R
#ubuntu    3339  1521 80 1356617 1974372 0 04:04 pts/1  Rl+    0:35 /usr/lib/R/bin/exec/R
#ubuntu    3339  1521 84 1356617 2075392 0 04:04 pts/1  Sl+    0:37 /usr/lib/R/bin/exec/R
#ubuntu    3339  1521 89 1356617 2174144 0 04:04 pts/1  Sl+    0:41 /usr/lib/R/bin/exec/R
#ubuntu    3339  1521 96 1356617 2182336 0 04:04 pts/1  Sl+    0:45 /usr/lib/R/bin/exec/R
#ubuntu    3339  1521 99 1356617 2299800 0 04:04 pts/1  Sl+    0:49 /usr/lib/R/bin/exec/R
#ubuntu    3339  1521 99 1356617 2302216 3 04:04 pts/1  Sl+    0:51 /usr/lib/R/bin/exec/R
#ubuntu    3339  1521 99 1356617 2384928 0 04:04 pts/1  Sl+    0:52 /usr/lib/R/bin/exec/R
#ubuntu    3339  1521 99 1356738 2479944 3 04:04 pts/1  Rl+    0:55 /usr/lib/R/bin/exec/R
#ubuntu    3339  1521 99 1366214 2609984 3 04:04 pts/1  Sl+    0:57 /usr/lib/R/bin/exec/R
#ubuntu    3339  1521 99 1372551 2673460 3 04:04 pts/1  Rl+    0:58 /usr/lib/R/bin/exec/R
#ubuntu    3339  1521 99 1382921 2737472 3 04:04 pts/1  Rl+    1:00 /usr/lib/R/bin/exec/R
#ubuntu    3339  1521 99 1391113 2770348 3 04:04 pts/1  Rl+    1:01 /usr/lib/R/bin/exec/R
#ubuntu    3339  1521 99 1390323 2767492 3 04:04 pts/1  Rl+    1:03 /usr/lib/R/bin/exec/R
#ubuntu    3339  1521 99 1396532 2792328 3 04:04 pts/1  Sl+    1:04 /usr/lib/R/bin/exec/R
#ubuntu    3339  1521 99 1420279 2887084 3 04:04 pts/1  Rl+    1:06 /usr/lib/R/bin/exec/R
#ubuntu    3339  1521 99 1414005 2862012 3 04:04 pts/1  Sl+    1:07 /usr/lib/R/bin/exec/R
#ubuntu    3339  1521 99 1436663 2952392 3 04:04 pts/1  Sl+    1:09 /usr/lib/R/bin/exec/R
#ubuntu    3339  1521 99 1453112 3017972 3 04:04 pts/1  Rl+    1:10 /usr/lib/R/bin/exec/R
#ubuntu    3339  1521 99 1448951 3001124 3 04:04 pts/1  Rl+    1:11 /usr/lib/R/bin/exec/R
#ubuntu    3339  1521 99 1435509 2947796 3 04:04 pts/1  Rl+    1:13 /usr/lib/R/bin/exec/R
#ubuntu    3339  1521 99 1440629 2968288 3 04:04 pts/1  Rl+    1:14 /usr/lib/R/bin/exec/R
#ubuntu    3339  1521 99 1457078 3033736 3 04:04 pts/1  Rl+    1:16 /usr/lib/R/bin/exec/R
#ubuntu    3339  1521 99 1477623 3115756 3 04:04 pts/1  Rl+    1:17 /usr/lib/R/bin/exec/R
#ubuntu    3339  1521 99 1484791 3135764 3 04:04 pts/1  Rl+    1:18 /usr/lib/R/bin/exec/R
#ubuntu    3339  1521 99 1489911 3155988 3 04:04 pts/1  Rl+    1:20 /usr/lib/R/bin/exec/R
#ubuntu    3339  1521 99 1466229 3061424 3 04:04 pts/1  Rl+    1:21 /usr/lib/R/bin/exec/R
#ubuntu    3339  1521 99 1518648 3258024 3 04:04 pts/1  Rl+    1:22 /usr/lib/R/bin/exec/R
#ubuntu    3339  1521 99 1475445 3085472 3 04:04 pts/1  Rl+    1:23 /usr/lib/R/bin/exec/R
#ubuntu    3339  1521 99 1479541 3101652 1 04:04 pts/1  Rl+    1:24 /usr/lib/R/bin/exec/R
#ubuntu    3339  1521 99 1521710 3270232 1 04:04 pts/1  Sl+    1:25 /usr/lib/R/bin/exec/R
#ubuntu    3339  1521 99 1521710 3270232 1 04:04 pts/1  Sl+    1:25 /usr/lib/R/bin/exec/R
#ubuntu    3339  1521 99 1521710 3270232 1 04:04 pts/1  Sl+    1:25 /usr/lib/R/bin/exec/R
#ubuntu    3339  1521 99 1521710 3270232 1 04:04 pts/1  Sl+    1:25 /usr/lib/R/bin/exec/R
#ubuntu    3339  1521 99 1521710 3270232 1 04:04 pts/1  Sl+    1:25 /usr/lib/R/bin/exec/R
#ubuntu    3339  1521 99 1521710 3270232 1 04:04 pts/1  Sl+    1:25 /usr/lib/R/bin/exec/R
#ubuntu    3339  1521 99 1521710 3270232 1 04:04 pts/1  Sl+    1:25 /usr/lib/R/bin/exec/R
#ubuntu    3339  1521 99 1521710 3270232 1 04:04 pts/1  Sl+    1:25 /usr/lib/R/bin/exec/R
#ubuntu    3339  1521 99 1521710 3270232 1 04:04 pts/1  Sl+    1:25 /usr/lib/R/bin/exec/R
#ubuntu    3339  1521 99 1521710 3270232 1 04:04 pts/1  Sl+    1:25 /usr/lib/R/bin/exec/R
#ubuntu    3339  1521 99 1521710 3270232 1 04:04 pts/1  Sl+    1:25 /usr/lib/R/bin/exec/R
#ubuntu    3339  1521 99 1521710 3270232 1 04:04 pts/1  Sl+    1:25 /usr/lib/R/bin/exec/R
#ubuntu    3339  1521 99 1521710 3270232 1 04:04 pts/1  Rl+    1:25 /usr/lib/R/bin/exec/R
#gc:
#ubuntu    3339  1521 99 1463147 3036172 1 04:04 pts/1  Sl+    1:25 /usr/lib/R/bin/exec/R
#ubuntu    3339  1521 98 1463147 3036172 1 04:04 pts/1  Sl+    1:25 /usr/lib/R/bin/exec/R
#ubuntu    3339  1521 97 1463147 3036172 1 04:04 pts/1  Sl+    1:25 /usr/lib/R/bin/exec/R
#ubuntu    3339  1521 96 1463147 3036172 1 04:04 pts/1  Sl+    1:25 /usr/lib/R/bin/exec/R
#ubuntu    3339  1521 95 1463147 3036172 1 04:04 pts/1  Sl+    1:25 /usr/lib/R/bin/exec/R

object.size(d)/1e3
402409.232 bytes

(3270232-76984)/402409.232
7.935325

(3036172-76984)/402409.232
7.353678


###########################


library(RODBC)

conn <- odbcDriverConnect("DSN=mysql")

d <-  sqlQuery(conn, "select * from d_10m")

gc()


#ubuntu    3666  1521  0 48961 33712   1 04:09 pts/1    S+     0:00 /usr/lib/R/bin/exec/R
#ubuntu    3666  1521  0 48961 33712   1 04:09 pts/1    S+     0:00 /usr/lib/R/bin/exec/R
#ubuntu    3666  1521  0 48961 33712   1 04:09 pts/1    S+     0:00 /usr/lib/R/bin/exec/R
#ubuntu    3666  1521  0 48961 33712   3 04:09 pts/1    S+     0:00 /usr/lib/R/bin/exec/R
#ubuntu    3666  1521  0 48961 33712   3 04:09 pts/1    S+     0:00 /usr/lib/R/bin/exec/R
#ubuntu    3666  1521  0 48961 33712   3 04:09 pts/1    S+     0:00 /usr/lib/R/bin/exec/R
#ubuntu    3666  1521  0 48961 33712   3 04:09 pts/1    S+     0:00 /usr/lib/R/bin/exec/R
#ubuntu    3666  1521  0 51689 44532   3 04:09 pts/1    R+     0:00 /usr/lib/R/bin/exec/R
#ubuntu    3666  1521  0 67368 106836  2 04:09 pts/1    S+     0:00 /usr/lib/R/bin/exec/R
#ubuntu    3666  1521  0 82121 165972  1 04:09 pts/1    S+     0:00 /usr/lib/R/bin/exec/R
#ubuntu    3666  1521  1 96937 225108  1 04:09 pts/1    S+     0:00 /usr/lib/R/bin/exec/R
#ubuntu    3666  1521  1 111950 285036 2 04:09 pts/1    R+     0:00 /usr/lib/R/bin/exec/R
#ubuntu    3666  1521  1 127318 346020 2 04:09 pts/1    R+     0:00 /usr/lib/R/bin/exec/R
#ubuntu    3666  1521  1 142521 407268 2 04:09 pts/1    S+     0:00 /usr/lib/R/bin/exec/R
#ubuntu    3666  1521  1 158085 469308 2 04:09 pts/1    S+     0:00 /usr/lib/R/bin/exec/R
#ubuntu    3666  1521  1 173442 531348 2 04:09 pts/1    S+     0:01 /usr/lib/R/bin/exec/R
#ubuntu    3666  1521  1 189061 593124 2 04:09 pts/1    S+     0:01 /usr/lib/R/bin/exec/R
#ubuntu    3666  1521  2 204421 654636 2 04:09 pts/1    R+     0:01 /usr/lib/R/bin/exec/R
#ubuntu    3666  1521  2 219342 714564 3 04:09 pts/1    S+     0:01 /usr/lib/R/bin/exec/R
#ubuntu    3666  1521  2 234085 773436 2 04:09 pts/1    S+     0:01 /usr/lib/R/bin/exec/R
#ubuntu    3666  1521  2 248995 832044 2 04:09 pts/1    S+     0:01 /usr/lib/R/bin/exec/R
#ubuntu    3666  1521  2 263555 890916 2 04:09 pts/1    S+     0:01 /usr/lib/R/bin/exec/R
#ubuntu    3666  1521  2 278148 949524 2 04:09 pts/1    R+     0:01 /usr/lib/R/bin/exec/R
#ubuntu    3666  1521  3 292728 1008396 2 04:09 pts/1   S+     0:02 /usr/lib/R/bin/exec/R
#ubuntu    3666  1521  3 307758 1067004 2 04:09 pts/1   S+     0:02 /usr/lib/R/bin/exec/R
#ubuntu    3666  1521  3 322171 1125876 2 04:09 pts/1   S+     0:02 /usr/lib/R/bin/exec/R
#ubuntu    3666  1521  4 427254 1397572 2 04:09 pts/1   R+     0:03 /usr/lib/R/bin/exec/R
#ubuntu    3666  1521  5 427254 1416004 2 04:09 pts/1   R+     0:04 /usr/lib/R/bin/exec/R
#ubuntu    3666  1521  7 427254 1436664 2 04:09 pts/1   R+     0:05 /usr/lib/R/bin/exec/R
#ubuntu    3666  1521  8 427254 1455096 2 04:09 pts/1   R+     0:06 /usr/lib/R/bin/exec/R
#ubuntu    3666  1521  9 427254 1475576 2 04:09 pts/1   R+     0:07 /usr/lib/R/bin/exec/R
#ubuntu    3666  1521 10 427254 1496056 2 04:09 pts/1   R+     0:08 /usr/lib/R/bin/exec/R
#ubuntu    3666  1521 11 427254 1514488 2 04:09 pts/1   R+     0:09 /usr/lib/R/bin/exec/R
#ubuntu    3666  1521 13 427254 1534968 2 04:09 pts/1   R+     0:10 /usr/lib/R/bin/exec/R
#ubuntu    3666  1521 14 469789 1712008 2 04:09 pts/1   R+     0:11 /usr/lib/R/bin/exec/R
#ubuntu    3666  1521 15 476154 1726792 2 04:09 pts/1   R+     0:12 /usr/lib/R/bin/exec/R
#ubuntu    3666  1521 16 564048 2011492 2 04:09 pts/1   R+     0:13 /usr/lib/R/bin/exec/R
#ubuntu    3666  1521 17 557862 2064228 2 04:09 pts/1   R+     0:14 /usr/lib/R/bin/exec/R
#ubuntu    3666  1521 18 525315 1918368 2 04:09 pts/1   R+     0:15 /usr/lib/R/bin/exec/R
#ubuntu    3666  1521 18 525315 1934232 2 04:09 pts/1   S+     0:15 /usr/lib/R/bin/exec/R
#ubuntu    3666  1521 18 525315 1934232 2 04:09 pts/1   S+     0:15 /usr/lib/R/bin/exec/R
#ubuntu    3666  1521 17 525315 1934232 2 04:09 pts/1   S+     0:15 /usr/lib/R/bin/exec/R
#ubuntu    3666  1521 17 525315 1934232 2 04:09 pts/1   S+     0:15 /usr/lib/R/bin/exec/R
#ubuntu    3666  1521 17 525315 1934232 2 04:09 pts/1   S+     0:15 /usr/lib/R/bin/exec/R
#ubuntu    3666  1521 17 525315 1934232 2 04:09 pts/1   S+     0:15 /usr/lib/R/bin/exec/R
#ubuntu    3666  1521 17 525315 1934232 2 04:09 pts/1   S+     0:15 /usr/lib/R/bin/exec/R
#ubuntu    3666  1521 16 525315 1934232 2 04:09 pts/1   S+     0:15 /usr/lib/R/bin/exec/R
#ubuntu    3666  1521 16 525315 1934232 2 04:09 pts/1   S+     0:15 /usr/lib/R/bin/exec/R
#ubuntu    3666  1521 16 525315 1934232 2 04:09 pts/1   S+     0:15 /usr/lib/R/bin/exec/R
#gc:
#ubuntu    3666  1521 16 398357 1426420 0 04:09 pts/1   S+     0:15 /usr/lib/R/bin/exec/R
#ubuntu    3666  1521 16 398357 1426420 0 04:09 pts/1   S+     0:15 /usr/lib/R/bin/exec/R
#ubuntu    3666  1521 16 398357 1426420 0 04:09 pts/1   S+     0:15 /usr/lib/R/bin/exec/R
#ubuntu    3666  1521 15 398357 1426420 0 04:09 pts/1   S+     0:15 /usr/lib/R/bin/exec/R
#ubuntu    3666  1521 15 398357 1426420 0 04:09 pts/1   S+     0:15 /usr/lib/R/bin/exec/R
#ubuntu    3666  1521 15 398357 1426420 0 04:09 pts/1   S+     0:15 /usr/lib/R/bin/exec/R


object.size(d)/1e3
282797.408 bytes

(2064228-34600)/282797.408
7.176968

(1426420-34600)/282797.408
4.921615


(2064228-34600)/402409.232
5.043691

(1426420-34600)/402409.232
3.458718


