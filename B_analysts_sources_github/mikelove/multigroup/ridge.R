set.seed(1)
n <- 120
x1 <- rep(c(0,1,0),c(n/3,n/3,n/3))
x2 <- rep(c(0,0,1),c(n/3,n/3,n/3))
y <- x1*rnorm(n,1) + x2*rnorm(n,2)
lm(y ~ 1 + x1 + x2)
g <- function(a,b) sum(dnorm(y, 0 + a*x1 + b*x2, 1, log=TRUE))
gv <- Vectorize(g)
s <- seq(from=-1,to=3,length=100)

pdf(file="ridge.pdf",width=5,height=5)
par(mar=c(4.5,4.5,1,1))
image(s,s,outer(s, s, gv),col=heat.colors(99),
      xlab=expression(beta[1]),ylab=expression(beta[2]))
contour(s,s,outer(s, s, gv),add=TRUE,nlevel=18)
abline(v=0,h=0)
pointz <- function(x,...) points(x[2],x[3],...)
pointz(coef(lm(y ~ 1 + x1 + x2)),pch=4,cex=2,lwd=2)
library(MASS)
c <- rep(1,n)
for (lambda in 10^seq(from=0, to=4, length=20)) {
  pointz(coef(lm.ridge(y ~ 1 + x1 + x2, lambda = lambda)),pch=1,cex=.5)
}
dev.off()
