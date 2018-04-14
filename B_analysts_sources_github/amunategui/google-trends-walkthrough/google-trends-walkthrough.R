setwd('/Users/manuelamunategui/Downloads/')

filename <- "report.csv"
con  <- file(filename, open = "r")

linecount <- 0
stringdata <- ""
while (length(oneLine <- readLines(con, n = 1, warn = FALSE)) > 0) {
        linecount <- linecount + 1
        
        if (linecount < 3) {
                filename <- paste0(filename,oneLine)     
        }
        
        # get headers at line 5
        if (linecount == 5) rowheaders = strsplit(oneLine, ",")[[1]]
        
        # skip firt 5 lines
        if (linecount > 5) {
                # break when there is no more main data
                if (gsub(pattern=",", x=oneLine, replacement="") == "") break
                
                stringdata <- paste0(stringdata,oneLine,"\n")
        }
}
close(con)

filename <- gsub(x=filename, pattern=' |;|:','_') 

newData <- read.table(textConnection(stringdata), sep=",", header=FALSE, stringsAsFactors = FALSE)
names(newData) <- rowheaders

head(newData)

newData$StartDate <- as.Date(sapply(strsplit(as.character(newData[,1]), " - "), `[`, 1))
newData$EndDate <- as.Date(sapply(strsplit(as.character(newData[,1]), " - "), `[`, 2))
newData$year <- sapply(strsplit(as.character(newData$StartDate), "-"), `[`, 1)
newData<- newData[c("StartDate", "EndDate", "beer", "wine", "year")]

head(newData)

plot(newData$StartDate, newData$beer, type='l', col='Blue')
lines(newData$StartDate, newData$wine, type='l', col='Red')

par(mfrow = c(1, 1))
# show box plots to account for seasonal outliers and stagnant trend
boxplot(beer~year, data=newData, notch=TRUE,
        col=(c("gold","darkgreen")),
        main="Yearly beer trend")

boxplot(wine~year, data=newData, notch=TRUE,
        col=(c("gold","darkgreen")),
        main="Yearly wine trend")

# shamelessly borrowed from aL3xa -
# http://stackoverflow.com/questions/4787332/how-to-remove-outliers-from-a-dataset
remove_outliers <- function(x, na.rm = TRUE, ...) {
        qnt <- quantile(x, probs=c(.25, .75), na.rm = na.rm, ...)
        H <- 1.5 * IQR(x, na.rm = na.rm)
        y <- x
        y[x < (qnt[1] - H)] <- NA
        y[x > (qnt[2] + H)] <- NA
        y
}

par(mfrow = c(1, 1))
# show box plots to account for seasonal outliers and stagnant trend
boxplot(beer~year, data=newData, notch=TRUE,
        col=(c("gold","darkgreen")),
        main="Yearly beer trend")

newData$wine_clean <- remove_outliers(newData$wine)
boxplot(wine_clean~year, data=newData, notch=TRUE,
        col=(c("gold","darkgreen")),
        main="Yearly wine trend")


# smooth both trends and plot them together
par(mfrow = c(1, 1))
plot(newData$StartDate, newData$beer, type='l', col='Blue')
lines(newData$StartDate, newData$wine_clean, type='l', col='Red')


library(ggplot2)
ggplot(newData,aes(x=StartDate)) +
        stat_smooth(aes(y = beer, group=1, colour="beer"), method=lm, formula = y ~ poly(x,1), level=0.95) +
        stat_smooth(aes(y = wine_clean, group=1, colour="wine"), method=lm, formula = y ~ poly(x,2), level=0.95) +
        geom_point (aes(y = beer, colour = "beer"), size=1) +
        geom_point (aes(y = wine_clean, colour ="wine"), size=1) +
        scale_colour_manual("Search Terms", breaks = c("beer", "wine"), values = c("blue","red")) +
        theme_bw() +
        xlab("year") +
        ylab("beverage") +
        ggtitle("US Wine Versus Beer on Google Trends")