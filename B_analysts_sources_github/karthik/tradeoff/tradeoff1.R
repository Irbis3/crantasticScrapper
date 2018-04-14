## This starts from my initial example code with some changes from Karthik
## 9/12/12
##library(varDev)

source('tradeoff1Code.R')
m <- 0.3
sA <- 0.7
F <- 3
a <- 1.0 # intercept
b <- -0.6 # slope
jps <- function(x) return((1/x)*(1/x)) ## shape = 1/CV^2. Set CV = .1:.1:1
CV <- seq(0.1, 1, by = 0.1)
juvshape <- jps(CV)

## undebug(solve.tradeoff.cor)
## debug(VD.tradeoff.curve.cor)
## solve.tradeoff.cor(a, b, seq(0.01, 0.99, length = 20), corr = 0.7, second.m.length = 50, VDtradeoffFunction = VD.tradeoff.curve.cor)

## ## When it gets stuck, there are essentially ZERO survivors happening, and the max iteration threshold is not being triggered.
## ## Temporary solution is to use a nearly 1 so that at max there are survivors.
## ## I added a solution to trap the case of zero survivors.

## js4 <- solve.tradeoff.cor(a, b, seq(0.01, 0.99, length = 20), corr = 0, second.m.length = 50, VDtradeoffFunction = VD.tradeoff.curve.juvgamma.cor, juvshape = 4)

## js4 <- solve.tradeoff.cor(a, -0.6, seq(0.01, 0.99, length = 20), corr = 0.5, second.m.length = 50, VDtradeoffFunction = VD.tradeoff.curve.juvgamma.cor, juvshape = juvshape[3])

## debug(VD.tradeoff.curve.juvgamma.cor)

## onerun <- VD.tradeoff.curve.juvgamma.cor(a, b, m.grid = seq(0.01, 0.99, length = 20), corr = 0, juvshape = 8)

## appears to work ok
growthSurvivalTradeoff <- list()
growthSurvivalDetails <- list()
growthSurvivalMatrixCase <- list()
bvalues <- c(-0.2, -0.4, -0.6)
for(ib in seq_along(bvalues)) {
  ans <- list()
  detailsrho <- list()
  b <- bvalues[ib]
  growthSurvivalMatrixCase[[paste('b=',b,sep='')]] <- solve.matrix.tradeoff.curve(a,b)
  for(rho in c(-.5, 0, .5)) {
    mstar <- numeric(length(juvshape))
    r <- numeric(length(juvshape))
    detailsjuvshape <- list()
    for(i in seq_along(juvshape)) {
      {
        setTimeLimit(cpu = 600, transient = TRUE)
        oneAns <- try(solve.tradeoff.cor(a, b, seq(0.01, 0.99, length = 20), corr = rho, second.m.length = 50, VDtradeoffFunction = VD.tradeoff.curve.juvgamma.cor, juvshape = juvshape[i]))
      }
      detailsjuvshape[[paste('juvshape=',juvshape[i],sep='')]] <- oneAns
      if(!inherits(oneAns, 'try-error')) {
        r[i] <- oneAns$second.opt$objective
        mstar[i] <- oneAns$second.opt$maximum
      } else {
        r[i] <- mstar[i] <- NA
      }
      writeLines('\n')
      writeLines(paste('b=',b,' rho=',rho,' juvshape[',i,']=',juvshape[i],' mstar=',mstar[i],' r=',r[i],sep=''))
      writeLines('\n')
    
    }
    ans[[paste("rho=",rho,sep='')]] <- data.frame(CV = CV, juvshape = juvshape, r = r, mstar = mstar)
    detailsrho[[paste('rho=',rho,sep='')]] <- detailsjuvshape
    writeLines(paste('Finished for rho=',rho,sep=''))
    print(ans[[paste("rho=",rho,sep='')]])
  }
  growthSurvivalTradeoff[[paste('b=',b,sep='')]] <- ans
  growthSurvivalDetails[[paste('b=',b,sep='')]] <- detailsrho
}

save(growthSurvivalTradeoff, growthSurvivalDetails, growthSurvivalMatrixCase, file = 'growthSurvivalResults.RData')
q('no')
