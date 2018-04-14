# Dean Attali
# Jan 15, 2014
# UBC STAT540
# small basic script to play with probability simulations

set.seed(1988)
# generate a few observations from a normal distribution
rnorm(sd = 100, mean = 30000, n = 8)

#======================
# create a 10x8 matrix with standard normal distribution as the values
nRows <- 10
nCols <- 8
trueMean <- 100
data <- matrix(rnorm(nRows * nCols, mean = trueMean), nrow = nRows)
rownames(data) <- sprintf("obs%02d", 1:nRows)
colnames(data) <- sprintf("samp%02d", 1:nCols)
print(data)

colMeans(data)  # calculate mean value per sample
apply(data, 2, mean)   # same result, different command

# calculate the mean of all the column (sample) means, and see how far it is from the true mean  
sampleMean <- mean(colMeans(data))
print(abs(sampleMean - trueMean))
# the sample is is pretty darn close to the true mean - yay, probability works!

#======================
# test out the weak law of large numbers: see what happens to the distribution of sample means as the sample
# size increases. Also see what happens to the variance

# generate random data normally distributed across a different number of samples with 10 rows
# and calculate their statistics
sampleSizes <- c(5, 15, 100, 1000, 10000)
nRows <- 10
trueMean <- 500
trueSd <- 50
trueVar <- trueSd^2
results <- matrix(nrow = length(sampleSizes), ncol = 6)
rownames(results) <- paste0("n", sampleSizes)
colnames(results) <- c("sampSize", "trueMean", "sampMean", "|zScore|", "trueVar", "sampVar")

for(size in sampleSizes) {
  datValues <- rnorm(n = size * nRows, sd = sqrt(trueVar), mean = trueMean)
  data <- matrix(datValues, ncol = size)
  rowname <- paste0("n", size)
  results[rowname, "sampSize"] <- size
  results[rowname, "trueMean"] <- trueMean
  results[rowname, "sampMean"] <- mean(colMeans(data))
  results[rowname, "|zScore|"] <- abs((results[rowname, "sampMean"] - trueMean) / trueSd)
  results[rowname, "trueVar"] <- trueVar
  results[rowname, "sampVar"] <- var(colMeans(data))
}

# we can clearly see a trend where with more samples the sample mean is closer to the true mean
# (also shown as a z-score getting closer to 0)
# the variance of the sample means seems to be fairly consistent... not sure what the statistical law there is

#=============================

# generate a large normal dataset, and see if the theoretical CDF at some value matches the empirical one 
trueMean <- 2000
trueSd <- 400
data <- rnorm(n = 10000, mean = trueMean, sd = trueSd)
threshold <- 2500
pnorm(q = threshold, mean = trueMean, sd = trueSd)  # theoretically what % of observations should be <= threshold
sum(data <= threshold) / length(data)   # empirically what % of observations are <= threshold
# they are very close!


# now do the same with a different distribution. Let's use poisson with lambda = 15 (on average I get
# 15 spam emails per day, so what is the chance that on some day I received <= 12 spam?)
lambda = 15
data <- rpois(10000, lambda = lambda)
threshold <- 12
ppois(threshold, lambda = lambda)
sum(data <= threshold) / length(data)
# again, very close!

# now see what effect sample size has
sampleSizes <- c(5, 10, 50, 100, 1000, 10000)
results <- matrix(nrow = length(sampleSizes), ncol = 2)
rownames(results) <- paste0("n", sampleSizes)
colnames(results) <- c("sampSize", "|diff(CDS)|")
for(size in sampleSizes) {
  data <- rpois(size, lambda = lambda)
  rowname <- paste0("n", size)
  results[rowname, "sampSize"] <- size
  trueCdf <- ppois(threshold, lambda)
  sampCdf <- sum(data <= threshold) / length(data)
  results[rowname, "|diff(CDS)|"] <- abs(trueCdf - sampCdf)
}
# sweet, it's clear that as sample size increases, the empirical CDF gets very close to the real one

# what's the chance of getting between 14-18 spams in one day?
maxSpam <- 18
minSpam <- 14
data <- rpois(10000, lambda = lambda)
ppois(maxSpam, lambda) - ppois(minSpam - 1, lambda)   # notice that we subtract 1 in this case!
sum(data >= minSpam & data <= maxSpam) / length(data)
# WOOOOOO!