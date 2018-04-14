# Basic numbers

# rows <- 240 # Number of observations columns <- 2 # Number of
# parametric components

# True parameters

# t_beta <- c(0.5, 2) t_sigma <- 1 t_lambda <- 1

# Fixing a seed

# set.seed(8142031)

# Design matrix X and g

# x1 <- rbinom(rows, 1, 0.5) x2 <- runif(rows, 0, 1) X <- cbind(x1,x2)

# t_knot1 <- 7 ts1 <- seq(0, 1, length = t_knot1) t_g1 <- 0.4 * sin(pi *
# ts1)

# t_knot2 <- 5 ts2 <- seq(0, 1, length = t_knot2) t_g2 <- exp(ts2)

# Building the first non-parametric variable

# BasisN <- function(n, knot) { N <- matrix(0, n, knot) m <- n/knot block
# <- matrix(1, m, 1) for (i in 1:knot) { l <- (i - 1) * m + 1 r <- i * m
# N[l:r, i] <- block } return(N) }

# s_N1 <- BasisN(rows, length(ts1)) x3 <- s_N1 %*% ts1 colnames(x3) <-
# 'x3'

# s_N2 <- BasisN(rows, length(ts2)) x4 <- s_N2 %*% ts2 colnames(x4) <-
# 'x4'

# Generating errors and data

# error <- robustloggamma::rloggamma(rows, 0, 1, t_lambda) y1 <- X
# %*%t_beta + t_sigma * error y2 <- y1 + s_N1 %*% t_g1 # sglg function
# with k=1 y3 <- y2 + s_N2 %*% t_g2 # sglg function with k=2

### CENSORED PART

# Generating errors and data

# s <- t_sigma^2 a <- 1/s t_ini1 <- exp(X %*% t_beta) * rgamma(rows,
# scale = s, shape = a) t_ini2 <- exp(X %*% t_beta + s_N1 %*% t_g1) *
# rgamma(rows,scale = s, shape = a) cens.time <- rweibull(rows, 0.3, 14)

# delta1 <- ifelse(t_ini1 > cens.time, 1, 0) delta2 <- ifelse(t_ini2 >
# cens.time, 1, 0) per.censo1 = 100 * mean(delta1) # Percentage of
# censoring

# obst1 = t_ini1 for(i in 1:rows) { if (delta1[i] == 1) { obst1[i] =
# cens.time[i] } }

# obst2 = t_ini2 for (i in 1:rows) { if (delta2[i] == 1) { obst2[i] =
# cens.time[i] } }

### Building the data frame example_sglg

# example_sglg <- data.frame(obst1, delta1, obst2, delta2, y1, y2, y3, X,
# x3, x4)

# devtools::use_data(example_sglg,internal=TRUE,overwrite = TRUE)

