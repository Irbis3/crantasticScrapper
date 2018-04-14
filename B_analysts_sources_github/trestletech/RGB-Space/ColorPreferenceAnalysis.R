
colorPreference <- read.csv("turk/output/Batch_790445_batch_results.csv", header=TRUE, stringsAsFactors=FALSE)
colorPreference <- colorPreference[,28:29]
colorPreference[,1] <- substr(colorPreference[,1], 44, 100)
colorPreference[,1] <- substr(colorPreference[,1],0,nchar(colorPreference[,1])-4)
colnames(colorPreference) <- c("palette", "rating")

prefList <- split(colorPreference[,2], colorPreference[,1])
boxplot(prefList)
vioplot(prefList[[1]],
        prefList[[2]],
        prefList[[3]],
        prefList[[4]],
        prefList[[5]],
        prefList[[6]],
        prefList[[7]],
        prefList[[8]],
        prefList[[9]],
        prefList[[10]],
        prefList[[11]],
        prefList[[12]],
        prefList[[13]],
        prefList[[14]],
        prefList[[15]],
        prefList[[16]],
        prefList[[17]],
        prefList[[18]])

fit <- anova(lm(colorPreference[,2] ~ as.factor(colorPreference[,1])))
pv <- (fit$"Pr(>F)")[1]

#linMod <- lm(colorPreference$rating ~ as.factor(colorPreference$palette))
#barplot(linMod$coefficients[-1], names.arg=c(1,10:18,2:9))

#identify the high-performing palettes
sort(sapply(prefList, mean), decreasing=TRUE)

#looks like it's the cool palettes that perform the best. Not sure how to quantify that, but we can play around with it later

r2 <- calcR2Sequential()
r2 <- unlist(r2)
names(r2) <- 1:18
avgs <- sapply(prefList, median)[order(as.integer(names(prefList)))]

#see if sticking tightly to a line in RGB space is aesthetically apealling.
anova(lm(avgs~r2))$"Pr(>F)"
plot(avgs~r2)
abline(lm(avgs~r2), col=3)

#first stab at quantifying how the cool palettes perform better
redness <- apply(sapply(allSequential, "[[", "R"),2,mean)
anova(lm(avgs~redness))$"Pr(>F)"
plot(avgs~redness)
abline(lm(avgs~redness), col=3)

#the more red the palette is, the worse it performs.


