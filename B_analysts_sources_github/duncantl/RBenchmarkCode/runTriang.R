source("genTriangular.R")

# see rexp in ../Bootstrap for similar inverse CDF generation.

x = rtriang(1e6)
hist(x, prob = TRUE)
summary(x)

