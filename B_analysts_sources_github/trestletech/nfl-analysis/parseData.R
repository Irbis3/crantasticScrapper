
#' Read in and bind all seasons' data available in the provided directory.
readData <- function(dataDir="./data"){
  files <- list.files(dataDir)
  
  allData <- NULL  
  for (f in files){
    thisFile <- read.csv(paste(dataDir, "/", f, sep=""), header=TRUE)
    
    #limit the current file only to the columns which already exist in the existing data
    if (!is.null(allData)){
      thisFile <- thisFile[,colnames(allData)]
    }
    allData <- rbind(allData, thisFile)   
  }
  allData  
}

#' Filter data to only those games involving a particular team
#' 
#' @param data The data as read in by readData()
#' @param team The team name. Teams typically use a three-letter abbreviation 
#' in this dataset.
#' @return Only those plays involving the specified team.
filterToTeam <- function(data, team){
  teamData <- data[data$off == team | data$def == team,]
  if (nrow(teamData) == 0){
    stop("Can't find any plays involving the specified team.")
  }
  teamData
}