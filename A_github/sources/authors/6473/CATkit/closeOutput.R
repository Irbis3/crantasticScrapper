closeOutput <-
function(file,output=list(Txt=FALSE,Doc=FALSE),console=FALSE,opar,ERR=FALSE,errMsg="",paramMsg=""){
if (output$Txt){    # print what would normally go to the console. A lot of debug info.
  sink()
}

if (output$Doc){
    fileName<-gsub(pattern='\b\b',replacement='\b',file)
    fileName<-gsub(pattern='pdf',replacement='rtf',fileName)
    #  may need to chg to wd:  http://www.r-statistics.com/2010/05/exporting-r-output-to-ms-word-with-r2wd-an-example-session/
    rtf<-RTF(fileName,width=11,height=8.5,omi=c(.5,.25,.5,.25), font.size=11)
  
    #  close final row.  Otherwise the very last cell wraps weirdly to the next line.
    matS<-data.frame(matrix(cbind(""),ncol=1))
    names(matS)<-c(NULL)
    addTable(rtf,matS,font.size=11,row.names=FALSE,NA.string="--",col.widths=c(.01))

  if (ERR){
    
    addParagraph(rtf,paste(fileName,paramMsg))
    addText(rtf,errMsg)
    addSessionInfo(rtf)
    done(rtf)                    # writes and closes the file
  }
}
if (!console){
  print(paste("Output has been saved to:",file))
  dev.off()
}

#remove read only list components 
opar$cin<-NULL
opar$cra<-NULL
opar$csi<-NULL
opar$cxy<-NULL
opar$din<-NULL
opar$page<-NULL
opar$pin<-NULL
par(opar)
}
