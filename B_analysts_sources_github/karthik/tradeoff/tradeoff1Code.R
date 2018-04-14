##library(varDev)

source('../varDev/R/distClass.R')
source('../varDev/R/fecundityClass.R')
source('../varDev/R/variableDevelopment.R')
source('../varDev/R/vdmClass.R')

## tradeoff:
sJ.from.m <- function(m, a, b) a + b*m ## b should be negative

tradeoff.curve <- function(a, b, m.grid = seq(0.01, 0.99, length = 20)) {
  sJ.grid <- lambda <- numeric(length(m.grid))
  for(i in seq_along(m.grid)) {
    m <- m.grid[i]
    sJ <- sJ.grid[i] <- sJ.from.m(m, a, b)
    A <- matrix(c( (1-m)*sJ, m*sJ, F, sA), nrow = 2) 
    lambda[i] <- max(eigen(A)$values)
  }
  data.frame(m = m.grid, sJ = sJ.grid, lambda = lambda)
}

solve.matrix.tradeoff.curve <- function(a, b) {
  objFun <- function(m) {
    sJ <- sJ.from.m(m, a, b)
    A <- matrix(c( (1-m)*sJ, m*sJ, F, sA), nrow = 2)
    max(eigen(A)$values)
  }
  O <- optimize(objFun, interval = c(1e-6, 1-1e-6), maximum = TRUE)
  O$maximum
}

#  Using the model.
VD.tradeoff.curve <- function(a, b, m.grid = seq(0.01, 0.99, length = 20)) {
  sJ.grid <- lambda <- numeric(length(m.grid))
  for(i in seq_along(m.grid)) {
    m <- m.grid[i]
    sJ <- sJ.grid[i] <- sJ.from.m(m, a, b)

    VDM <- suppressMessages(VD.model(2,
                    marginal.durations = list(VD.dist("geomp1", list(prob = m)),
                      VD.dist("geomp1", list(prob = (1-sA)))),
                    marginal.death.times = list(VD.dist("geomp1", list(prob = (1-sJ))),
                  VD.dist("infinite"))))
    controls(VDM) <- VD.controls(2, batch.size = 50000, alive.target = 100000, dead.target = c(100000, 0), max.dead = 200000, max.total = 1e6)

    VDS <- VD.run(VDM)
    dev.table <- compile.dev.table(VDS)
    mean.fec <- calc.average.surv.rep.by.age(dev.table, F = F) ## F from global
    r <- VD.solve.euler(mean.fec)
    lambda[i] <- exp(r)
  }
  data.frame(m = m.grid, sJ = sJ.grid, lambda = lambda)
}

# adding var in juv development.

VD.tradeoff.curve.juvgamma <- function(a, b, m.grid = seq(0.01, 0.99, length = 20), juvshape) {
  ## Additional argument juvshape is the shape of the gamma for juvenile duration
  sJ.grid <- lambda <- numeric(length(m.grid))
  for(i in seq_along(m.grid)) {
    m <- m.grid[i]
    sJ <- sJ.grid[i] <- sJ.from.m(m, a, b)

    lambdaJ <- -log(1-m)
    meanjuv <- 1/lambdaJ ## mean of the exponential
    juvscale <- meanjuv / juvshape

    VDM <- suppressMessages(VD.model(2,
                    marginal.durations = list(VD.dist("gamma", list(shape = juvshape, scale = juvscale)),
                      VD.dist("geomp1", list(prob = (1-sA)))),
                    marginal.death.times = list(VD.dist("geomp1", list(prob = (1-sJ))),
                  VD.dist("infinite"))))
    controls(VDM) <- VD.controls(2, batch.size = 50000, alive.target = 100000, dead.target = c(100000, 0), max.dead = 200000, max.total = 1e6)
  VDS <- VD.run(VDM)
    dev.table <- compile.dev.table(VDS)
    mean.fec <- calc.average.surv.rep.by.age(dev.table, F = F) ## F from global
    r <- VD.solve.euler(mean.fec)
    lambda[i] <- exp(r)
  }
  data.frame(m = m.grid, sJ = sJ.grid, lambda = lambda)
}

# Finally, with correlation.
VD.tradeoff.curve.cor <- function(a, b, m.grid = seq(0.01, 0.99, length = 20), corr) {
  my.gauss.cov <- matrix(c(1,corr,corr,1), nrow = 2)
  sJ.grid <- lambda <- numeric(length(m.grid))
  for(i in seq_along(m.grid)) {
    m <- m.grid[i]
    sJ <- sJ.grid[i] <- sJ.from.m(m, a, b)

    VDM <- suppressMessages(VD.model(2,
                    marginal.durations = list(VD.dist("geomp1", list(prob = m)),
                      VD.dist("geomp1", list(prob = (1-sA)))),
                    marginal.death.times = list(VD.dist("geomp1", list(prob = (1-sJ))),
                  VD.dist("infinite")), gauss.cov = my.gauss.cov))
    VDS <- VD.run(VDM)
    dev.table <- compile.dev.table(VDS)
    mean.fec <- calc.average.surv.rep.by.age(dev.table, F = F) ## F from global
    r <- VD.solve.euler(mean.fec)
    lambda[i] <- exp(r)
  }
  data.frame(m = m.grid, sJ = sJ.grid, lambda = lambda)
}

VD.tradeoff.curve.juvgamma.cor <- function(a, b, m.grid = seq(0.01, 0.99, length = 20), juvshape, corr) {
  my.gauss.cov <- matrix(c(1,corr,corr,1), nrow = 2)
  sJ.grid <- lambda <- numeric(length(m.grid))
  for(i in seq_along(m.grid)) {
    m <- m.grid[i]
    sJ <- sJ.grid[i] <- sJ.from.m(m, a, b)

    lambdaJ <- -log(1-m)
    meanjuv <- 1/lambdaJ ## mean of the exponential
    juvscale <- meanjuv / juvshape

    
    VDM <- suppressMessages(VD.model(2,
                    marginal.durations = list(VD.dist("gamma", list(shape = juvshape, scale = juvscale)),
                      VD.dist("geomp1", list(prob = (1-sA)))),
                    marginal.death.times = list(VD.dist("geomp1", list(prob = (1-sJ))),
                  VD.dist("infinite")), gauss.cov = my.gauss.cov))
    ##controls(VDM) <- VD.controls(2, batch.size = 5000, alive.target = 10000, dead.target = c(10000, 0), max.dead = 20000, max.total = 1e5)
    controls(VDM) <- VD.controls(2, batch.size = 50000, alive.target = 100000, dead.target = c(100000, 0), max.dead = 200000, max.total = 1e6)
    VDS <- VD.run(VDM)
    ## This should really be handled by the VD code:
    if(VDS@simulation.summary$total.alive[1] > 0) {
      ##
      dev.table <- compile.dev.table(VDS)
      mean.fec <- calc.average.surv.rep.by.age(dev.table, F = F) ## F from global
      r <- VD.solve.euler(mean.fec)
      lambda[i] <- exp(r)
    } else {
      lambda[i] <- 0
    }
  }
  data.frame(m = m.grid, sJ = sJ.grid, lambda = lambda)
}


## summary: a good way to do this is two steps:
## 1. narrow the range of the answer
## 2. run more points
## 3. use gam instead of smooth.spline.

solve.tradeoff.cor <- function(a, b, first.m.grid, second.m.length, corr, VDtradeoffFunction, threshold = 0.9, ...) {
  require(mgcv)
  first.pass <- VDtradeoffFunction(a, b, m.grid = first.m.grid, corr = corr, ...)
  first.gam <- gam(lambda ~ s(m), data = first.pass)
  first.objfun <- function(x) predict(first.gam, list(m = x))
  first.opt <- optimize(first.objfun, range(first.m.grid), maximum = TRUE)
  first.max <- first.opt$objective
  ratios <- (fitted(first.gam) - min(fitted(first.gam)))/(first.max - min(fitted(first.gam)))
  second.range <- first.m.grid[range(which(ratios > threshold))]
  second.m.grid <- rep(seq(second.range[1], second.range[2], length = ceiling(second.m.length/2)), each = 2)

  second.pass <- VDtradeoffFunction(a, b, m.grid = second.m.grid, corr = corr, ...)
  second.gam <- gam(lambda ~ s(m), data = second.pass)
  second.objfun <- function(x) predict(second.gam, list(m = x))
  second.opt <- optimize(second.objfun, range(second.m.grid), maximum = TRUE)
  list(first.pass = first.pass, first.opt = first.opt, second.pass = second.pass, second.opt = second.opt, opt = second.opt)
}
