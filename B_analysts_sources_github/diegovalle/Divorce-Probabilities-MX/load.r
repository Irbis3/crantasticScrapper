########################################################
#####       Author: Diego Valle Jones
#####       Website: www.diegovalle.net
#####       Date Created: Sun Jul 11 11:03:57 2010
########################################################
#Load and Clean the divorce data as downloaded from the
#INEGI

div <- read.csv("data/divorce-data.csv", skip = 3)

remCommas <- function(df){
    df <- as.numeric(gsub(",", "", df))
}

cols <- 2:ncol(div)
div <- data.frame(sapply(div, remCommas))

total.div <- data.frame(div[1,cols])
total.div

length <- subset(div, X <= 2007 & X >= 1920)
names(length)[1] <- c("Year")
write.csv(length, "output/marriages-length.csv", row.names = FALSE)

div <- subset(div, X <= 2007 & X >= 1993)
div$X.1 <- NULL

mar <- read.csv("data/marriage-data.csv", skip = 3)
mar$X.1 <- NULL
mar <- sapply(mar, remCommas)
mar <- mar[1, (2:ncol(mar))]


pyearly <- data.frame(apply(div[,cols], 2, function(x) x / mar))


x <- data.frame(t(pyearly))
x[is.na(x)] <- 0
pcumsum <- data.frame(t(cumsum(x)))

shiftCols <- function(df) {
    div.probs <- df
    div.probs[TRUE] <- NA
    div.probs[1, ] <- df[1, ]

    for(i in 2:nrow(df)) {
        div.probs[i,(1:(ncol(df)-i+1))] <- df[i,i:ncol(df)]
    }
    row.names(div.probs) <- c(1993:2007)
    names(div.probs) <- 0:14
    div.probs
}


div.pyearly <- shiftCols(pyearly)
div.probs <- shiftCols(pcumsum)

write.csv(div.pyearly, "output/divorce-probs-byyear.csv")
write.csv(div.probs, "output/divorce-probs.csv")

