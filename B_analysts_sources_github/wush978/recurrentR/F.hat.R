library(recurrentR)

F.hat.prototype <- function(obj) {
  s <- recurrentR:::s(obj)
  d <- recurrentR:::d(obj)
  N <- sapply(s, function(s) {
    retval <- 0
    for(i in seq_along(obj@y)) {
      if(s > obj@y[i]) next
      retval <- retval + sum(obj@t[[i]] <= s)
    }
    retval
  })
  function(t) {
    index <- which(s > t)
    if (length(index) == 0) return(1)
    prod(1 - d[index] / N[index])
  } 
}

data(obj.list)
for(i in seq_along(obj.list)) {
  obj <- obj.list[[i]]
  F.hat0 <- F.hat.prototype(obj)
  F.hat1 <- obj$F.hat
  s <- recurrentR:::s(obj)
  F.hat0.s <- sapply(s, function(s) F.hat0(s))
  F.hat1.s <- F.hat1(s)
  stopifnot(isTRUE(all.equal(F.hat0.s, F.hat1.s)))
  F.hat0.y <- sapply(obj@y, function(y) F.hat0(y))
  F.hat1.y <- F.hat1(obj@y)
  stopifnot(isTRUE(all.equal(F.hat0.y, F.hat1.y)))
}

for(i in seq_along(obj.list)) {
  obj <- obj.list[[i]]
  F.hat1 <- obj$F.hat
  F.hat1.y <- F.hat1(obj@y)
  m <- sapply(obj@t, length)
  stopifnot(m[F.hat1.y == 0] == 0)
}

function() {
  N0 <- local({
    s <- recurrentR:::s(obj)
    d <- recurrentR:::d(obj)
    N <- sapply(s, function(s) {
      retval <- 0
      for(i in seq_along(obj@y)) {
        if(s > obj@y[i]) next
        retval <- retval + sum(obj@t[[i]] <= s)
      }
      retval
    })
    N
  })
  N1 <- local({
    s <- recurrentR:::s(obj)
    d <- recurrentR:::d(obj)
    N <- cumsum(d)
    y.i <- order(obj@y)
    m <- sapply(obj@t, length)
    N + recurrentR:::eval_N(s, obj@y[y.i], m[y.i])
  })
  stopifnot(isTRUE(all.equal(N0, N1)))
  x <- recurrentR:::s(obj)
  y <- local({
    s <- recurrentR:::s(obj)
    d <- recurrentR:::d(obj)
    N <- cumsum(d)
    y.i <- order(obj@y)
    m <- sapply(obj@t, length)
    N <- N + recurrentR:::eval_N(s, obj@y[y.i], m[y.i])
    p <- 1 - d/N
    rev(cumprod(rev(p)))
  })
  y0 <- sapply(x, function(x) F.hat0(x))
  isTRUE(all.equal(tail(y, length(x)-1), head(y0, length(x)-1)))
}

function() {
  j <- which(F.hat1.y == 0)[which(m[which(F.hat1.y == 0)] != 0)]
  s <- recurrentR:::s(obj)
  d <- recurrentR:::d(obj)
  N <- local({
    N <- cumsum(d)
    y.i <- order(obj@y)
    m <- sapply(obj@t, length)
    N + recurrentR:::eval_N(s, obj@y[y.i], m[y.i])
  })
  head(d)
  head(N)
  obj@y[j]
  s[1]
  s[2]
  d
  N
  obj@t[[j]]
  m[j]
}
