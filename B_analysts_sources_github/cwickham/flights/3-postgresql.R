library(plyr)
library (RPostgreSQL)

drv <- dbDriver("PostgreSQL")

# == create db locally == #
# create db once
system("createdb ontime")

# connect from R
nconn <- dbConnect(drv, dbname = "ontime")  

# create flights table
rs <- dbSendQuery(nconn, "create table flights (
  Year int,
  Month int,
  DayofMonth int,
  DayOfWeek int,
  DepTime  int,
  CRSDepTime int,
  ArrTime int,
  CRSArrTime int,
  UniqueCarrier varchar(7),
  FlightNum int,
  TailNum varchar(8),
  ActualElapsedTime int,
  CRSElapsedTime int,
  AirTime int,
  ArrDelay int,
  DepDelay int,
  Origin varchar(3),
  Dest varchar(3),
  Distance int,
  TaxiIn int,
  TaxiOut int,
  Cancelled int,
  CancellationCode varchar(1),
  Diverted varchar(1),
  CarrierDelay int,
  WeatherDelay int,
  NASDelay int,
  SecurityDelay int,
  LateAircraftDelay int
)")

years <- 1987:2013

# unzip the files
unzip_files <- function(year){
  cat("Unzipping ", year, "\n")
  file <- paste("yearly/", year, ".csv", sep = "")
  if(!file.exists(file)){
    system(paste("bunzip2 -k ", file, ".bz2", sep = ""))  
  }
}
l_ply(years, unzip_files)

base <- "COPY flights FROM '/Users/wickhamc/Documents/flights/yearly/"
end <- ".csv' WITH DELIMITER ',' CSV HEADER NULL 'NA';"

calls <- paste(base, years, end, sep = "")

rs <- dbSendQuery(nconn, "SET CLIENT_ENCODING TO LATIN1;")

l_ply(calls, function(sql_call) dbSendQuery(nconn, sql_call), .progress = "text", .inform = TRUE)

rs <- dbSendQuery(nconn, 
  "create index date on flights(year, month, dayofmonth);")
rs <- dbSendQuery(nconn, 
  "create index origin on flights(origin);")
rs <- dbSendQuery(nconn, 
  "create index dest on flights(dest);")
rs <- dbSendQuery(nconn, 
  "create index uniquecarrier on flights(uniquecarrier);")
rs <- dbSendQuery(nconn, 
  "create index year on flights(year);")
rs <- dbSendQuery(nconn, 
  "create index flightnum on flights(flightnum);")
rs <- dbSendQuery(nconn, 
  "analyze;")
rs <- dbSendQuery(nconn, 
  "vacuum;")

# # Then in a clean session this should work
# library(dplyr)
# 
# ontime <- src_postgres("ontime")
# 
# flights <- tbl(ontime, "flights")
# as.tbl(head(flights))

