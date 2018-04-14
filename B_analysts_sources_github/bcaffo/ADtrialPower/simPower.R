##simulates power for a randomized two treatment trial with random slopes and intercepts
##Brian Caffo 2012
library(lme4)
library(mvtnorm)

##bet0 intercept, fine to just set this to 0. It's assumed constant across the groups do to the randomization but is estimated
##beta1 slope
##perd the percent decrease in the slope due to the treatment expressed as a proportion
##sigmau0 sd of the RI
##sigmau1 sd of the RS
##cor12 correlation between the RI and random slope 
##sigma sd of the residual error
##method the method of fitting rirs = random intercept, random slope, rs = random slope alone, ri = random intercept alone, h = Holland's model
##if method = t this is two group t using the endpoints. You can do power.t.test for this, it's only there to check things
##n = number of subjects
##mtime the time of measurements c(0, .5, 1) = measurements at baseline, 6 months and 1 year
##nosim number of simulations
##alpha type I error rate
##alternative numeric 1 or 2 sided or more if your want Bon. corrections. For example 2-sided 2-arm Bonf. corrected set alpha = 4
##direction greater (anything else is treated as less) direction to look for a significant effect

##return values as a matrix
#r rejection yes or no
#t tvalue
#b beta values
#vc variance components with variances square rooted and covariances left as is
#s residual variance

simPower <- function(beta0 = 0, 
                     beta1, 
                     perd, 
                     n, 
                     sigmau0, 
                     sigmau1, 
                     cor12, 
                     sigma,
                     method = "rirs",
                     mtime = c(0, .5, 1), 
                     nosim = 100, 
                     alpha = .05, 
                     alternative = 2, 
                     direction = "greater"){
  ##note the 2 is for a two sided test
  alpha <- alpha / alternative 
  ##make sure mtime was put in the correct order
  mtime <- sort(mtime)
  ##the number of measurements per subject
  m <- length(mtime)
  ##time variable for group1 concatenated across subjects
  time1 <- rep(mtime, n)
  time2 <- time1
  ##the time variable for the whole simulated data set
  time <- c(time1, time2)
    
  ##start the simulation
  out <- sapply(1 : nosim, 
    function(i) {
      ##generate the residual errrors 
      e1 <- rnorm(n * m, mean = 0, sd = sigma)
      e2 <- rnorm(n * m, mean = 0, sd = sigma)

      ##random intercept no random slope case
      if (sigmau1 == 0 & sigmau0 > 0){
        u1 <- rnorm(n, mean = 0, sd = sigmau0)
        u2 <- rnorm(n, mean = 0, sd = sigmau0)
        y1 <- beta0 + time1              * beta1 + rep(u1, rep(m, n)) + e1
        y2 <- beta0 + time2 * (1 - perd) * beta1 + rep(u2, rep(m, n)) + e2
      }
      ##random slope and fixed intercept case
      else if (sigmau1 > 0 & sigmau0 == 0){
        u1 <- rnorm(n, mean = 0, sd = sigmau1)
        u2 <- rnorm(n, mean = 0, sd = sigmau1)
        y1 <- beta0 + time               * beta1 + time1 * rep(u1, rep(m, n)) + e1
        y2 <- beta0 + time2 * (1 - perd) * beta1 + time2 * rep(u2, rep(m, n)) + e2
      }
      ##random slope and intercept case
      else if (sigmau1 > 0 & sigmau0 > 0){
        ##generate the random slopes u1 for control and u2 for treated
        cov12 <- cor12 * sigmau1 * sigmau0
        Sigma <- matrix(c(sigmau0 ^ 2, cov12, cov12, sigmau1 ^ 2), 2)
        u1 <- rmvnorm(n, mean = c(beta0, beta1             ), sigma = Sigma)
        u2 <- rmvnorm(n, mean = c(beta0, beta1 * (1 - perd)), sigma = Sigma)
        u1rep <- cbind(rep(u1[,1], rep(m, n)), rep(u1[,2], rep(m, n)))
        u2rep <- cbind(rep(u2[,1], rep(m, n)), rep(u2[,2], rep(m, n)))        
        x1 <- cbind(1, time1)
        x2 <- cbind(1, time2)
        y1 <- apply(x1 * u1rep, 1, sum) + e1
        y2 <- apply(x2 * u2rep, 1, sum) + e2
      }
      
      ##the response
      y <- c(y1, y2)
      ##the group variable
      group <- rep(0 : 1, c(n * m , n * m))
      ##the subject indicator
      subject <- rep(1 : (2 * n), rep(m, 2 * n))  
      
      ##fit the case for random intercepts and random slopes
      if (method == "rirs") {
        fit <- lmer(y ~ time + time : group + (time | subject) )
        b <- attributes(summary(fit))$coef[,1]
        s <- attributes(summary(fit))$sigma
        temp <- as.vector(VarCorr(fit)$subject)
        vc <- c(sqrt(temp[1]), temp[2], sqrt(temp[4]))
        t <- attributes(summary(fit))$coef[3,3]  
      }
      ##fit the case for the random intercept only
      else if (method == "ri") {
        fit <- lmer(y ~ 1 + time + time : group + (1 | subject) )
        b <- attributes(summary(fit))$coef[,1]
        s <- attributes(summary(fit))$sigma
        vc <- attr(VarCorr(fit)$subject, "stddev")
        t <- attributes(summary(fit))$coef[3,3]  
      }
      ##fit the case for the random slope only
      else if (method == "rs") {
        fit <- lmer(y ~ 1 + time + time : group + (-1 + time | subject) )
        b <- attributes(summary(fit))$coef[,1]
        s <- attributes(summary(fit))$sigma
        vc <- attr(VarCorr(fit)$subject, "stddev")
        t <- attributes(summary(fit))$coef[3,3]          
      }
      ##specifically fit Holland's model
      else if (method == "h"){
        ##obtain the ids of of all of the basline measurements and repeat them to
        ##get a vector of repeated basline measurements
        blids <- rep(seq(1, 2 * m * n, by = m), rep(m, 2 * n))
        ##subtract out the basline
        tildey <- y - y[blids]
        fit <- lmer(tildey ~ -1 + time + time : group + (-1 + time | subject), subset = time != 0)
        b <- attributes(summary(fit))$coef[,1]
        s <- attributes(summary(fit))$sigma
        vc <- attr(VarCorr(fit)$subject, "stddev")
        t <- attributes(summary(fit))$coef[2,3]
      }
      ##the two group t test subtracting baseline and latest followup
      else if (method == "t"){
        blids <- rep(seq(1, 2 * m * n, by = m), rep(m, 2 * n))
        mids <- rep(seq(m, 2 * m * n, by = m), rep(m, 2 * n))
        tildey <- y[mids] - y[blids]
        yc <- tildey[group == 0]
        yt <- tildey[group == 1]
        b <- c(mean(yc), mean(yt), mean(yt) - mean(yc))
        s <- sqrt( .5 * var(yc) + .5 * var(yt))
        se <- s * sqrt(2 / n)
        t <- (mean(yt) - mean(yc)) / se        
        vc <- NA
      }
      else stop("incorrect method specification")
      
      if (direction == "greater") r <- as.integer( t > qnorm(1 - alpha) )
      else r <- as.integer( t < qnorm(alpha) )
      
      return( c(r, b, vc, s) )
    }
  )
  rval <- apply(t(out), 2, mean)[1] * 100
  return(rval)
}
