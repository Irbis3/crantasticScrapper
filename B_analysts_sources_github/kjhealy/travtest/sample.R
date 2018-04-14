## ----Setup, include=FALSE, results='hide', warning=FALSE-----------------

opts_chunk$set(dev=c("png","pdf"),
               fig.width=6,
               fig.height=5,
               dpi=300,
               fig.show="hold",
               fig.lp="fig:",
               cache=TRUE,
               par=TRUE,
               echo=TRUE,
               message=FALSE,
               warning=FALSE)


## ---- figure-1-----------------------------------------------------------

correlatedValue <- function(x, r){
  r2 = r^2
  ve = 1-r2
  SD = sqrt(ve)
  e  = rnorm(length(x), mean=0, sd=SD)
  y  = r*x + e
  return(y)
}

set.seed(10)
x <- rnorm(200)
y <- correlatedValue(x=x, r=.5)

z <- correlatedValue(x=y, r=0.8)
z <- factor(z>0 & sample(z, 200))

x <- x + 10
y <- y + 10


plot(x, y,
     xlab = "Tea",
     xlab = "Biscuits")


