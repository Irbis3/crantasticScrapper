print.dcens <- function(x, ...){

  cat("Quantiles for SL survival function:\n\n")
  print(format(as.data.frame(x$quantilesSL),digits=4,nsmall=4),print.gap=4,quote=F)
  cat("\nQuantiles for ST survival function:\n\n")
  print(format(as.data.frame(x$quantilesST),digits=4,nsmall=4),print.gap=4,quote=F)
  cat("\nQuantiles for SR survival function:\n\n")
  print(format(as.data.frame(x$quantilesSR),digits=4,nsmall=4),print.gap=4,quote=F)
	
  cat("\n");
  cat(paste("Proportion of exact data:\t\t",format(x$exactData*100,digits=4),"%\n"))
  cat(paste("Proportion of left censored data:\t",format(x$leftCensored*100,digits=4),"%\n"))
  cat(paste("Proportion of right censored data:\t",format(x$rightCensored*100,digits=4),"%\n\n"))

}