## Applied Nonparametric Econometrics
## Simulation and comparsion between parametric and nonparametric statistical tests on non-i.i.d. sample

set.seed(123)
iid <- function(n,test) {
  d1 <- rnorm(n)
  d2 <- rnorm(n)
  test(d1,d2)
}

ma <- function(n,ma,test) {
  sd <- 1/sqrt(1+ma^2)
  d1 <- arima.sim(model=list(ma=ma), n=n, sd=sd)
  d2 <- arima.sim(model=list(ma=ma), n=n, sd=sd)
  test(d1,d2)
}

sim <- function(n,fun) {
  data.frame(t(sapply(1:n,fun)))
}

quantiles <- function(result,probs) {
  rq <- sapply(1:ncol(result),
               function(i)quantile(result[,i],probs=probs))
  colnames(rq) <- colnames(result)
  rq
}

powers <- function(result,q) {
  lens <- sapply(1:ncol(result),
                 function(i) {
                   len1 <- length(result[result[,i]<=q[1,i],i])
                   len2 <- length(result[result[,i]>q[2,i],i])
                   (len1+len2)/nrow(result)
                 })
  names(lens) <- colnames(result)
  lens
}

test <- function(nsim,nsample,size,theta,test.fun,order=1) {
  probs <- c(size/2,1-size/2)
  iidr <- sim(nsim,function(i) iid(nsample,test.fun))
  iidrq <- quantiles(iidr,probs)
  marp <- NULL
  marpf <- function(x) {
    mar <- sim(nsim,
               function(i) ma(nsample,rep(x,order),test.fun))
    powers(mar,iidrq)
  }
  marps <- sapply(theta, marpf)
  marps <- t(marps)
  rownames(marps) <- theta
  marps
}

tests <- function(d1,d2) {
  mw <- wilcox.test(d1,d2,paired=F)
  wt <- wilcox.test(d1,d2,paired=T)
  ks <- ks.test(d1,d2)
  t <- t.test(d1,d2)
  t.paired <- t.test(d1,d2,paired=T)
  c(mw=mw$statistic,
    wt=wt$statistic,
    ks=ks$statistic,
    unpaired=t$statistic,
    paired=t.paired$statistic)
}

plot.tests <- function(results1,results2,titles) {
  cols <- ncol(results1)
  par(mfrow=c(ceiling(cols/2),2),new=TRUE)
  for(i in 1:cols) {
    plot(x=rownames(results1),
         y=results1[,i],
         type='l',
         main=paste(colnames(results1)[i],titles[1]),
         xlab="theta",
         ylab="pi(theta)")
    plot(x=rownames(results2),
         y=results2[,i],
         type='l',
         main=paste(colnames(results2)[i],titles[2]),
         xlab="theta",
         ylab="pi(theta)")
  }
}

plot.tests <- function(results,titles) {
  cols <- ncol(results[[1]])
  nr <- length(results)
  par(mfrow=c(ceiling(cols/nr),nr))
  for(i in 1:cols) {
    for(j in 1:nr) {
      plot(x=rownames(results[[j]]),
           y=results[[j]][,i],
           type='l',
           main=paste(colnames(results[[j]])[i],titles[j]),
           xlab="theta",
           ylab="pi(theta)",
           cex.main=1.5,
           cex.lab=1.5,
           cex.axis=1.2)
    }
  }
}

testr1 <- test(1000,100,0.01,seq(-0.9,0.9,0.1),tests)
testr2 <- test(1000,500,0.01,seq(-0.9,0.9,0.1),tests)
testr3 <- test(1000,500,0.01,seq(-0.9,0.9,0.1),tests,order=2)
plot.tests(list(testr1,testr2),c("(n=100)","(n=500)"))
plot.tests(list(testr2,testr3),c("(order=1)","(order=2)"))
