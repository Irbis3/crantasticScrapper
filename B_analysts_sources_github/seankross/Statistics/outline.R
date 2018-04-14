# Termin 2
# What is needed to answer the questions?


df$cond <- factor(df$cond,labels=c("mit","ohne"))
df$sex <- factor(df$sex,labels=c("w","m"))
df$mood <- as.numeric(df$mood)
is.numeric(df$mood)
df[c(85,132,138,139,150),"mood"] <- 5
df <- df[-which(is.na(df$rt)),]
summary(df)
table(df$sex,df$cond)

aggregate(df$mood,list(sex=df$sex),mean)
aggregate(df$rt,list(cond=df$cond),mean)

