
draw.several.CVvsMstar <- function(TR, draw.tradeoff.fun = NULL ) {
  par(mfrow = c(2, 2), mar = c(3,3.5,4,0.5)+0.1)
  if(!is.null(draw.tradeoff.fun)) draw.tradeoff.fun(TR)
  for(itr in seq_along(TR)) {
    draw.one.CVvsMstar(TR, itr)
  }
}

draw.one.CVvsMstar <- function(TR, itr) {
  with(do.call('rbind',TR[[itr]]), plot(mstar ~ CV, type = 'n', xlab = '', ylab = '', main = '', bty = 'l', tcl = 0.5))
  for(irho in seq_along(TR[[itr]])) {
    with(TR[[itr]][[irho]], points(mstar ~ CV, type = 'b', lty = itr, pch = irho))
  }
  legend('topleft', lty = rep(itr,3), pch = seq_along(TR[[itr]]), legend = names(TR[[itr]]))
  mtext('CV of juvenile development time', 1, 2)
  mtext('Optimal growth rate', 2, 2)
  mtext(paste('Tradeoff with', names(TR)[itr]),3,1)
}

draw.growthFecundity.tradeoff <- function(TR) {
  m <- seq(0, 1, length=100)
  b <- substr(names(TR), 3, 5)
  a <- 6.0

  F <- list()
  for(ib in seq_along(b)) {
    thisb <- as.numeric(b[ib])
    F[[ib]] <- a + thisb*m
  }
  plot(rep(m, length(b)), do.call('rbind',F), type = 'n', xlab ='', ylab = '', main = '', bty = 'l', tcl=0.5)
  for(ib in seq_along(b)) {
    points(m, F[[ib]], type='l', lty = ib)
  }
  legend('bottomleft', lty = seq_along(b), legend = names(TR))
  mtext('Juvenile growth rate',1,2)
  mtext('Fecundity',2,2)
  mtext('Three growth-fecundity tradeoffs',3,1)
}

draw.growthSurvival.tradeoff <- function(TR) {
  m <- seq(0, 1, length=100)
  b <- substr(names(TR), 3, 10)
  a <- 1.0

  F <- list()
  for(ib in seq_along(b)) {
    thisb <- as.numeric(b[ib])
    F[[ib]] <- a + thisb*m
  }
  plot(rep(m, length(b)), do.call('rbind',F), type = 'n', xlab ='', ylab = '', main = '', bty = 'l', tcl=0.5)
  for(ib in seq_along(b)) {
    points(m, F[[ib]], type='l', lty = ib)
  }
  legend('bottomleft', lty = seq_along(b), legend = names(TR))
  mtext('Juvenile growth rate',1,2)
  mtext('Juvenile survival',2,2)
  mtext('Three growth-survival tradeoffs',3,1)
}
