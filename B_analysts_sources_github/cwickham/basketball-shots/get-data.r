# get shots for all LA Laker games

library(XML)
library(plyr)

url <- "http://www.basketballgeek.com/downloads/2009-2010/"

file_page <- htmlParse(url)
lines <- xmlSApply(xmlRoot(file_page)[[2]], xmlValue)
files <- lines[grepl(".csv", lines)]

# download all files
base_url <- "http://www.basketballgeek.com/downloads/2009-2010/"

# you need a data directory
res <- sapply(files, function(f) {
  download.file(paste(base_url, f, sep = ""), 
    destfile = paste("data/", f, sep = ""))})
any(res != 0)

# find LA Lakers games
files <- dir("data")
LA <- files[grepl("LAL", files)]

get_shots <- function(file){
  game_id <- substr(file, 1, 15)
  tmp <- read.csv(paste("data/", file, sep = ""))
  tmp$game_id <- game_id
  subset(tmp, etype == "shot")
}

LA_shots <- ldply(LA, get_shots)

shots_sub <- LA_shots[, c("player", "type", "result", "x", "y", "points", "time")]
write.csv(shots_sub, file = "LAshots.csv", row.names = FALSE)
