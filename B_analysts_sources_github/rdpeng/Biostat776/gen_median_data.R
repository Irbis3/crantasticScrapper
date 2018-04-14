## Generate test vectors for median function

set.seed(134234)

x1 <- rnorm(100)
x2 <- rexp(1000, 1/20)
x3 <- rnorm(749, 0, 10)
x3[sample(749, 2)] <- NA
x4 <- rbeta(85, 4, 1)
is.na(x3) <- rbinom(85, 1, 0.3) > 0
x5 <- rpois(5, 10)

save(x1, x2, x3, x4, x5, file = "../data/median_testdata.rda")
