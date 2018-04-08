########################################################################################################
########################################################################################################
####                                   PATHChange Package
########################################################################################################
########################################################################################################
################################### PATHChangeDat function   ###########################################
########################################################################################################

PATHChangeDat <- function(eDat, DataSet, NumbSample, Genes, HistComp, hc, writeRDS, destDIR){
  DataSet <- toupper(DataSet)
  tf <- tempfile() ; td <- tempdir()
  
  GSENumb <- as.numeric(str_extract(DataSet, "[0-9]+"))
  a<- if(str_length(GSENumb)>=4){paste0("GSE", substr(GSENumb, 1, str_length(GSENumb)-3), "nnn")}else{paste("GSE", "nnn", sep="")}
  
  url <- paste("ftp.ncbi.nlm.nih.gov/geo/series",a, DataSet, "matrix", paste(DataSet, "series_matrix.txt.gz", sep="_"), sep="/")
  download.file(url, destfile=paste0("./", paste(DataSet, "series_matrix.txt.gz", sep="_")), method="libcurl",mode = "wb")
  MatrixGeo<-read.table(paste(DataSet, "series_matrix.txt.gz", sep="_"), header = FALSE, sep = "\t", col.names = paste0("V",1:(NumbSample+1)), fill = TRUE, strip.white = FALSE)
  file.remove(paste(DataSet, "series_matrix.txt.gz", sep="_"))
  
  geo_accession<-as.matrix(MatrixGeo[str_detect(MatrixGeo[,1], "ID_REF"),])
  title<-as.vector(as.matrix(MatrixGeo[str_detect(MatrixGeo[,1], "!Sample_title"),]))
  
  description<- cbind(title=title, geo_accession=as.vector(geo_accession))[-1,]
  detectLevels <- str_extract_all(title[-1], regex("[a-z]+|[[:alnum:]]+\\.*[[:digit:]]*", TRUE))
  level <- vector()
  for (i in 1:length(detectLevels)){level[i] <- str_c(detectLevels[[i]], collapse = " ")}
  description[,"title"]<-level; level <- unique(level)
  
  if(HistComp==TRUE){
    combinations <- NamesComb <- list()
    count = 0
    repeat{
      HistologyComp <- readline(c("Choose tissues to compare [ENTER]"))
      count <- count +1
      print(level)
      Ctrl <- readline(c("Please, choose the control groups: "))
      Control<-print(as.matrix(description[str_detect(description[,"title"], Ctrl),]))
      print(level)
      Exp <- readline(c("Please, choose the Experimental groups: "))
      Experiment <- print(as.matrix(description[str_detect(description[,"title"], Exp),]))
      combinations[[count]] <- rbind(Control, Experiment)
      NamesComb[[count]] <- paste(c(Ctrl, Exp), sep="", collapse=" ")
      if(response <- readline(c("Would you like to compare another tissues? (yes/no) ")) == "no")break;
    }}else{Control <- print(as.matrix(description[str_detect(description[,"title"], hc[1]),])); 
    Experiment <- print(as.matrix(description[str_detect(description[,"title"], hc[2]),]));
    combinations <- list(rbind(Control, Experiment));
    NamesComb <- paste(hc, collapse=" ")}
  
  
  GenesSet<- read.table(Genes, header=TRUE)
  eDat<-read.table(eDat, header = TRUE, sep = "/")
  data <- merge(eDat, GenesSet, by.x ="Symbol", by.y ="ApprovedSymbol")
  data <- aggregate(data[,3:dim(data)[2]], by = list(Symbol=data$Symbol), FUN = function(x) mean(as.numeric(as.character(x)))) 
  
  MeanData <- list()
  for(k in 1:length(combinations)){
    colnames(data) <- c("Symbol", as.vector(geo_accession)[-1])
    comb <- as.character(combinations[[k]][,"geo_accession"])
    dat <- matrix(0, dim(data)[1], length(comb))
    for(i in 1:length(comb)){
      for(j in 2:length(data)){
        if(colnames(data)[j]==comb[i]){dat[,i] <- data[,j]}
      }
    }
    MeanData[[k]]<-cbind.data.frame(Symbol=data[,1], cbind(Control=rowMeans(dat[,1:dim(Control)[1]]), Experiment=rowMeans(dat[,-c(1:dim(Control)[1])])))
  }
  names(MeanData)<-NamesComb
  list.save(MeanData, file.path(td,"MeanData.rds"))
  if(writeRDS==TRUE){list.save(MeanData, paste(destDIR, "MeanData.rds", sep="/"))}
  return(MeanData)
}