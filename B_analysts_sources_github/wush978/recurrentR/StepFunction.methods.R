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