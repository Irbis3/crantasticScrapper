step_integrate <- function(g, f, a, b) {
  stopifnot("stepfun" %in% class(f))
  e <- environment(f)
  x <- e[["x"]]
  y <- c(e[["f"]], e[["y"]])
  p <- diff(y)
  index <- which(x >= a & x <= b)
  if (length(index) == 0) return(0)
  sum(g(x[index]) * p[index])
}

step_integrate.StepFunction <- function(g, f, a, b) {
  x <- f$x
  # 	index <- which(x >= a & x <= b)
  index <- .Call("substring_index", x, a, b)
  if (length(index) == 0) return(0)
  y <- f$y
  p <- diff(y)
  sum(.Call("StepFunction_sort_call", g, x[index], PACKAGE="recurrentR") * p[index])
}

evalqOnLoad({
  
  for(i in c("^", "+", "-", "*", "/")) {
    (function(i) {
      operator <- i
      setMethod(i,
                signature(e1 = StepFunction, e2 = StepFunction),
                function (e1, e2) 
                {
                  x <- c(e1$x, e2$x)
                  x <- unique(x)
                  x <- sort(x)
                  x.eval <- c(min(x) - 1, x)
                  y <- get(operator, envir=baseenv())(e1$sort_call(x.eval), e2$sort_call(x.eval))
                  new(StepFunction, x, y)
                }
      )
      invisible(NULL)
    })(i)
    (function(i) {
      operator <- i
      setMethod(i,
                signature(e1 = StepFunction),
                function (e1, e2) 
                {
                  x <- e1$x
                  x.eval <- c(min(x) - 1, x)
                  y <- get(operator, envir=baseenv())(e1$sort_call(x.eval), e2)
                  new(StepFunction, x, y)
                }
      )
      invisible(NULL)
    })(i)
  }
  
  
})