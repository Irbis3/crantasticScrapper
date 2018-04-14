#' Reads and parses the provided turk results file. 
#' @param file The file containing the turk results
#' @param writeResults Boolean representing whether or not the results should be written to 
#' a CSV file (useful for uploading to Turk)
#' @return A list containing the delta (error) values for all experiments from all palettes.
readAndParseResults <- function(file, writeResults){
  
  acc <- read.csv(file, header=TRUE, stringsAsFactors=FALSE)
  acc <- acc[,28:31]
  acc[,1] <- substr(acc[,1], 48, 100)
  acc[,1] <- substr(acc[,1],0,nchar(acc[,1])-4)
  colnames(acc) <- c("palette", "x", "y", "answer")
  head(acc)
  
  acc$answer <- gsub("\"","",acc$answer)
  
  #data curation
  
  #loop through each row and check value
  delta <- array(dim=nrow(acc))
  for (i in 1:nrow(acc)){
    thisRow <- acc[i,]  
    delta[i] <- abs(as.numeric(thisRow$answer) - mat[thisRow$x, thisRow$y])
  }
  
  response <- read.csv(file, header=TRUE, stringsAsFactors=FALSE)
  
  
  response$Reject[which(is.na(delta))] <- "You must provide an integer response with no other data, such as \"1\" or \"15\". You cannot provide two different values, a range of values, or leave the field blank."
  response$Reject[which(delta>4)] <- "The value you provided is too far away from the expected value. Please read the instructions carefully to understand how to read the heatmap before entering data."
  
  
  response$Approve[is.na(response$Reject)] <- "x"
  
  if(writeResults){  
    write.csv(response, file="results.csv", row.names=FALSE, na="")
  }
  
  #/curation
  
  acc <- acc[is.na(response$Reject),]
  
  delta <- list()
  for (i in 1:nrow(acc)){
    thisRow <- acc[i,]    
    d <- abs(as.numeric(thisRow$answer) - mat[thisRow$x, thisRow$y])
    delta[[thisRow$palette]] <- c(delta[[thisRow$palette]], d)
    acc$delta[i] <- d
  }
  
  delta <- delta[order(as.integer(names(delta)))]
  return(list(delta=delta, acc=acc))
}

turkResults <- readAndParseResults("../turk/output/Batch_933667_batch_results.csv", FALSE)





# r2 <- unlist(calcR2Sequential())
# names(r2) <- 1:18
# 
# anova(lm(delta~r2))$"Pr(>F)"
# plot(delta~r2)
# abline(lm(delta~r2), col=3)
# 
# 
# 
# pl <- calcPathLength()
# anova(lm(delta~pl))$"Pr(>F)"
# plot(delta~pl)
# abline(lm(delta~pl), col=3)
# 
# #first stab at quantifying how the cool palettes perform better
# redness <- apply(sapply(allSequential, "[[", "R"),2,mean)
# anova(lm(delta~redness))$"Pr(>F)"
# plot(delta~redness)
# abline(lm(delta~redness), col=3)
# 
# 
# plot(avgs, delta, type="n", xlab="Aesthetics", ylab="Average error")
# text(avgs, delta, 1:18)
# symbols(avgs, delta, circles=pl^10, add=TRUE)
# 
# 
# 
