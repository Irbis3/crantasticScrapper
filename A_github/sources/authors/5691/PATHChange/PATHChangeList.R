########################################################################################################
########################################################################################################
####                                   PATHChange Package
########################################################################################################
########################################################################################################
###################################  PATHChangeList function  ##########################################
########################################################################################################

PATHChangeList <- function(filePathway, writeRDS, destDIR){
  Pathway <- read.table(filePathway, header=T)
  colnames(Pathway)<-c("Pathway", "ApprovedSymbol")
  unique.path <- as.character(unique(Pathway$Pathway))
  
  path <- list()
  for (i in 1:length(unique.path)){
    path[i] <- list(Pathway[Pathway$Pathway == unique.path[i], ])
  }
  list.save(path, file.path(tempdir(),"path.rds"))
  if(writeRDS==TRUE){list.save(path, paste(destDIR, "path.rds", sep="/"))}
  return(path)
}