df <- read_csv("tmp/tweetout.csv")
plot(df$Lon, df$Lat)
library(RODBC)

channel <- odbcDriverConnect('driver={SQL Server};server=SEEDSQL1;database=Twitter;trusted_connection=true')
channel <- odbcDriverConnect("")

DB1 <- RODBC

sqlTables(channel, tableType = "TABLE")


sel <- sqlQuery(channel, "SELECT TOP 1000 [TweetId]
                ,[Text]
                ,[TwitterId]
                ,[Language]
                ,[Location]
                ,[NumFollowers]
                ,[NumTweets]
                ,[Lon]
                ,[Lat]
                ,[DateCreated]
                ,[MigratedFrom]
                FROM [Twitter].[dbo].[tblTweetStoreArchive]")

sel <- sqlQuery(channel, "SELECT * FROM [Twitter].[dbo].[tblTweetStoreArchive]
                WHERE lat < 54 AND lat > 53 AND lon < 0 AND lon > -2")

plot(sel$Lon, sel$Lat)


library(leaflet)

leaflet() %>% addTiles() %>% addPopups(sel$Lon[1:100], sel$Lat[1:100], popup = sel$Text[1:100])

dplyr::src_sql("Twitter", con = channel)



head(sel)

library(rgdal)


?odbcConnect