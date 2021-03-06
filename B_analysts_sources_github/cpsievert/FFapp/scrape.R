library(XML)
library(plyr)
pre <- "http://www.fantasypros.com/nfl/rankings/"
pos <- c("qb", "rb", "wr", "te", "flex", "qb-flex", "k", "dst", "idp", "dl", "lb", "db")
ppr.pre <- "http://www.fantasypros.com/nfl/rankings/ppr-"
ppr.pos <- c("rb", "wr", "te", "flex", "qb-flex")
urls <- paste0(pre, pos, ".php")
ppr.urls <- paste0(ppr.pre, ppr.pos, ".php")
urlz <- c(urls, ppr.urls)
poz <- c(pos, ppr.pos)
ppr <- c(rep("N", length(urls)), rep("Y"), length(ppr.urls))
dats <- NULL
experts <- NULL
for (i in seq_along(urlz)) {
  dat <- readHTMLTable(urlz[i], stringsAsFactors=FALSE)
  dats[[i]] <- cbind(dat$data, Category=poz[i], PPR=ppr[i])
  #if ("experts" %in% names(dat)) 
  experts[[i]] <- cbind(dat$experts, Category=poz[i])
}
names(dats[[6]]) <- names(dats[[5]]) #names are up messed on 6th df
names(dats[[17]]) <- names(dats[[16]]) #names are up messed on 17th df
df <- rbind.fill(dats)
names(df)[1] <- "Rank"
names(df)[2] <- "Player"
names(df)[10] <- "Matchup"
df2 <- df[!is.na(df$Player), ] # grab non defensive-special teams rankings
x <- strsplit(df2$Player, " \\(")
df2$Player <- sapply(x, "[[", 1) #overwrite player column so that it just includes player names (without matchups)
df2$Matchup <- sapply(x, "[[", 2) #Overwirte matchup (which be NA before this point)
final <- rbind(df2, df[is.na(df$Player), ])
final$Matchup <- gsub("\\(", "", final[,9])
final$Matchup <- gsub("\\)", "", final[,9])

names(final) <- gsub(" ", "", names(final))
num <- c("Rank", "Best", "Worst", "Ave", "StdDev")
for (i in num) final[,i] <- as.numeric(final[,i])
names(final) <- tolower(names(final))
write.csv(final, file="wk1.csv", row.names=FALSE)

