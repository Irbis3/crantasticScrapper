library(plyr)

vals <- rnorm(10^6, 100)

sample_mean_var <- function(n=5){
  tt <- sample(vals, size = n)
  data.frame(n=n, mean=mean(tt), sd=sd(tt), stringsAsFactors = FALSE)
}

df <-
  ldply(c(5, 10, 15, 20), sample_mean_var)
