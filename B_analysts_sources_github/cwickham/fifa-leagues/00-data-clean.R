# adds columns, 
# calculates connections required to plotting line segments

library(stringr)

leagues <- read.csv("data/leagues.csv", stringsAsFactors = FALSE, comment = "#")

leagues$stage <- factor(rep(c("Group Stage", "Round of 16", "Quarterfinals",
    "Semifinals", "Finals"), c(10, 10, 10, 10, 8)), 
  levels = c("Group Stage", "Round of 16", "Quarterfinals",
    "Semifinals", "Finals"))
leagues$country <- str_split_fixed(leagues$league, "[()]", 3)[, 2]

leagues$rank <- factor(rep(1:10, length = 48), levels = 10:1)

leagues$region <- "Europe"
na <- c("Liga MX (Mexico)","Primera Division (Costa Rica)","Major League Soccer (U.S.)")
leagues$region[leagues$league %in% na] <- "North America"

leagues$labels <- with(leagues, paste(league, " ", percent, "%", sep = ""))

sa <- c("Primera Division (Argentina)",  
  "Serie A (Brazil)")
leagues$region[leagues$league %in% sa] <- "South America"

write.csv(leagues, "data/league-clean.csv", row.names = FALSE)

