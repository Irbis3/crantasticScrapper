# We start with a sample from an Exponential distribution.
x = rexp(30, 2)

# We are interested in the sampling distribution of the median statistic.
T = median(x)

# Compare the density of our sample values and our true distribution of the random variable.
if(FALSE) {
plot(density(x))
curve(dexp(x, 2), 0, 1.1*max(x), add = TRUE, col = "red")

abline(v = c(true = qexp(.5, 2), sample = T), col = c("black", "red"))
}

B = 10000

# Now use a non-parametric bootstrap for the median.
T.star = replicate(B,  {
                   x.star = sample(x, length(x), replace = TRUE)
                   median(x.star)
                 })

plot(density(T.star)); abline(v = T); rug(T.star)

T.star.np = T.star

# Distribution of bootstrap medians are "blocky"
# If we know x are exponential, then can generate new ones for each bootstrap sample
# to get more randomness and smoother/less blocky.
# So a parametric bootstrap
# We need the maximum likelihood estimates (MLEs) of the Exponential parameter
lambda.hat = 1 / mean(x)

T.star = replicate(B,  {
                   g.star = rexp(30, lambda.hat)
                   median(g.star)
                 })
lines(density(T.star), col = "red"); abline(v = mean(T.star), col = "red"); rug(T.star, col = "red")


#####################

# Let's simulate the median from the actual distribution.
T.true = replicate(B, median(rexp(30, 2)))
mean(T.true)
qexp(.5, 2)
sd(T.true)
plot(density(T.true), main = "true sampling distribution of the median", xlab = "median")
abline(v = c(mean(T.true), mean(T.star.np), mean(T.star)), col = c("blue", "green", "red")) 
legend("topright", c("actual", "parametric", "non-parametric"), lty = 1, col = c("blue", "green", "red"))
