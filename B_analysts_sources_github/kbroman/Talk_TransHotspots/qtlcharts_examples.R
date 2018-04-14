# load library
library(qtlcharts)

# example of iplot
n <- 500
x <- rnorm(n)
grp <- sample(1:3, n, replace=TRUE)
y <- x*grp*2 + rnorm(n)
iplot(x, y, grp)

# example of iplotMap
data(hyper)
iplotMap(hyper)

# example of iplotScanone
hyper <- calc.genoprob(hyper, step=1)
out <- scanone(hyper)
iplotScanone(out)

# iplotScanone with effects
iplotScanone(out, hyper, chr=c(1, 4, 6, 7, 15))

# iplotScanone with dotplots
iplotScanone(out, hyper, chr=c(1, 4, 6, 7, 15), pxgtype="raw")
