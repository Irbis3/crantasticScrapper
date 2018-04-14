# load libraries
library("stringr")
library("dplyr")

# Cleanup, remove old database folders
"xxxx"

# download ITIS data
itis_db_url <- 'http://www.itis.gov/downloads/itisSqlite.zip'
itis_db_path <- '~/.taxize_local/itisSqlite.zip'
itis_db_path_file <- '~/.taxize_local/itisSqlite'
download.file(itis_db_url, destfile = itis_db_path)
unzip(itis_db_path)
dirs <- list.dirs(full.names = TRUE)
dir_date <- dirs[ dirs != "." ]
db_path <- list.files(dir_date, pattern = ".sqlite", full.names = TRUE)

# initialize connection to databasecreate database
itis_db <- src_sqlite(db_path)

# get a table
dat <- tbl(itis_db, "taxonomic_units")

# query
dat %>% select(tsn)
