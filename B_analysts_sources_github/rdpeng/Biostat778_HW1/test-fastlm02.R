## Remove missing values

library(Homework1)
library(datasets)
data(airquality)
op <- options(scipen = 5)

X <- cbind(1, data.matrix(airquality[, -1]))
y <- airquality$Ozone

fit <- fastlm(X, y, na.rm = TRUE)
print(drop(fit$coefficients))
print(fit$vcov)

options(op)
