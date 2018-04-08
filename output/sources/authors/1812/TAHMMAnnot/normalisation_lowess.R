normalisation_lowess<-function(fileIN="lame.txt",MeanDyeswap=TRUE,Lowess=TRUE,graph=TRUE,name.chr=paste("CHR",c("01","02","03","04","05","C","M"),sep=""),underscore=FALSE,sep.write="\t",sep.read="\t")
{  
  name.chr=sort(name.chr)
  path = attr(as.environment(match("package:TAHMMAnnot",search())),"path")

  cat("############################# \n")
  cat("FORMATTING FILES \n")
  cat("############################# \n")
###lecture des fichiers d'annotation par chromosome et remplir une liste
  

    ###lecture du fichier fileIN et des fichiers pair
  file = read.table(fileIN,sep=sep.read)
  cat("file to analyse :  \n")
  print(file)
  cat("\n")
  
  for(i in 1:length(file[,1]))
    {
      if (graph == TRUE)
        pdf(file = paste(as.character(file[i,1]), ".pdf", sep = ""))
      green<-read.table(paste(file[i,1],"_532.pair",sep=""),header=TRUE,skip=1,sep=sep.read)
      red<-read.table(paste(file[i,1],"_635.pair",sep=""),header=TRUE,skip=1,sep=sep.read)
      #enlever le underscore des ID
      if (underscore==TRUE)
      {
        green$PROBE_ID<-paste(substr(green$PROBE_ID,1,5),substr(green$PROBE_ID, 7, 24),sep="")
      }
      for (n in 1:length(name.chr))
	{
	  index.g<-grep(name.chr[n],green$PROBE_ID)
          index.r<-grep(name.chr[n],green$PROBE_ID)
          
          data<-data.frame(ID=green$PROBE_ID[index.g],GREEN=log2(green$PM[index.g]),RED=log2(red$PM[index.r]))
             
	      IS1 = data$GREEN[order(data$ID)]	
	      IS2 = data$RED[order(data$ID)] 
	      ID = sort(data$ID)
	      data2 = data.frame(ID,IS1,IS2)
  	 

          fileOUT<-paste("RawData_",file[i,1],"_",name.chr[n],".txt",sep="")
	  write.table(data2,fileOUT,row.names=FALSE,sep=sep.write)
###Norm lowess
          if(Lowess == TRUE)
            {
              A<-(data2$IS1+data2$IS2)/2
              M<-(data2$IS2-data2$IS1)
              ordre <- order(A)
              yfit1 <- lowess(A, M, f = 0.3)$y[order(ordre)]
              Mcorrec <- M - yfit1
              IS1<-(2*A+Mcorrec)/2
              IS2<-(2*A-Mcorrec)/2
              if (graph == TRUE)
                {
                  par(mfrow = c(1, 1))
                  plot(A, M, xlab = "A", ylab = "M", main = "M-A plot : raw data", 
                       pch = 20, cex = 0.5)
                  lines(lowess(A, M, f = 0.3), col = 2)
                  plot(A,Mcorrec, main = "M-A plot after a global lowess normalization", 
                       xlab = "A", ylab = "M", pch = 20, cex = 0.5)
                  lines(lowess(A,Mcorrec, f = 0.3), col = 3)
                }
              fileOUT<-paste("NormData_",file[i,1],"_",name.chr[n],".txt",sep="")
	      data2 = data.frame(ID=data2$ID,IS1,IS2)	
              write.table(data2,fileOUT,row.names=FALSE,sep=sep.write)
            }
        }
      dev.off()
    }
  cat("Files by array and by chromosome with ordered probes are created \n")
  if(MeanDyeswap == TRUE)
    {
      if(Lowess == TRUE)
        file<-system("ls NormData*",TRUE)
      else
        file<-system("ls RawData*",TRUE)
      for (n in 1:length(name.chr))
        {
          f1<-read.table(file[grep(name.chr[n],file)[1]],header=TRUE)
          f2<-read.table(file[grep(name.chr[n],file)[2]],header=TRUE)
          res<-data.frame(ID=f1[,1],IS1=(f1$IS1+f2$IS2)/2,IS2=(f1$IS2+f2$IS1)/2)
          fileOUT<-paste("MeanDyeswap_",name.chr[n],".txt",sep="")
          write.table(res,fileOUT,row.names=FALSE,sep=sep.write)
        }
      if( Lowess == TRUE)
        cat("Files of normalized data with average over the dye-swap are created \n")	
      else
        cat("Files of raw data with average over the dye-swap are created \n")	
    }
}
