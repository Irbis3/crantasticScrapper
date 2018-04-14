# source: http://data.gov.tw/node/8116
data <- read.csv("102年遊憩區年表32.csv", header=TRUE, stringsAsFactor=FALSE, 
                 check.names = FALSE)
head(data)
colnames(data)
data <- data[c(2 : 16)]
data <- na.omit(data)
for(i in 3:15) {
  name <- names(data)[i]
  data[[name]] <- as.integer(gsub(",", replacement="", data[[name]], fixed=TRUE))
}

# Remove repeated values
index_set <- rep(NA, nrow(data))
for(i in seq_along(unique(data[,1]))) {
  name <- unique(data[,1])[i]
  index <- which(data[,1] == name)
  tmp <- data[index,3:15]
  if (nrow(tmp) == 1) {
    index_set[i] <- index
    next
  }
  if (!all(apply(tmp, 2, sd) == 0)) browser()
  index_set[i] <- index[1]
}
data <- data[na.omit(index_set),]
data <- data[data[,15] != 0,]
# K-mean

freq <- data[,3:14]
rownames(freq) <- data[,1]
freq <- freq[rowSums(freq) != 0,]
freq <- sweep(freq, 1, FUN="/", STATS=rowSums(freq))
total <- data[,15]
names(total) <- data[,1]
freq.total <- apply(data[,3:14], 2, sum) / sum(apply(data[,3:14], 2, sum))

k <- 12
cluster <- kmeans(freq, k, iter.max=1000)
matplot(t(cluster$centers), type="l", col = 1:k, lty = 1:k, ylim= c(0, 1),
        xlim = c(1, 14))
lines(1:12, freq.total, lwd=2, col="gray")
legend("topright", paste(1:k, cluster$size), col=1:k, lty = 1:k)
cluster$size
source("inverted_index.R")
inverted_index(cluster$cluster)$`2`
total["奧萬大國家森林遊樂區\nAowanda National Forest Recreation Area"]
freq["奧萬大國家森林遊樂區\nAowanda National Forest Recreation Area",]
