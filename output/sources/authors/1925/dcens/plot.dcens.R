plot.dcens <- function(x, fun="ST", type="s", col="red", lwd=2, bty="l", ylim=c(0,1), xlab="t", ylab, main, ...){

  #if (!inherits(x, "dcens")) 
  #      stop("x must be an object of class 'dcens'")

  if(!(fun %in% c("ST","SL","SR"))){
	cat("Invalid function.")
	return()	
  }
  
  if(missing(main)){
	switch(fun,
	       ST = main <- expression(paste(S[T]," survival function")),
		   SL = main <- expression(paste(S[L]," survival function")),
		   SR = main <- expression(paste(S[R]," survival function")))	   
  }	
  
  if(missing(ylab)){
	switch(fun,
	       ST = ylab <- expression(paste(S[T]," (t)")),
		   SL = ylab <- expression(paste(S[L]," (t)")),
		   SR = ylab <- expression(paste(S[R]," (t)")))	    
  }

  plot(x=x$survs[,c("t",fun)], type=type, col=col, lwd=lwd, bty=bty, ylim=ylim, xlab=xlab, ylab=ylab, main=main, ...)
  
#  switch(fun,
#		 ST = plot(x$survs[,"t"], x$survs[,fun], type, col, lwd, bty, ylim, xlab, ylab, main, ...),
#		 SL = plot(x$survs[,"t","SL")], type, col, lwd, bty, ylim, xlab, ylab, main, ...),
#		 SR = plot(x$survs[,c("t","SR")], type, col, lwd, bty, ylim, xlab, ylab, main, ...))

}
