pmc <-
function(data, fileName, colNames, GrpID = NA, alpha = 0.05, header=TRUE, sep="\t", Output=list(Doc=TRUE,Txt=FALSE), functionName="", title=""){
  
  if (missing("data")){   #  if a matrix has not been passed, read from the file into matrix
    data <- read.delim2(fileName, sep=sep, header=header, quote="",stringsAsFactors=FALSE, blank.lines.skip = TRUE)
  }
  str(data)       #  display matrix
  if (!missing("colNames")){      #  if column names have been passed, read them into matrix
    colnames(data) <- colNames
  }
  
  vars <- tolower(colnames(data))
  idx <- which(vars %in% c("pr", "mesor", "amp", "phi"))
  colnames(data)[idx] <- vars[idx]
  
  if(!all(c("pr", "mesor", "amp", "phi") %in% vars)){
    if(!"pr" %in% vars){
      data$pr <- NA
    }
    if(!"mesor" %in% vars){
      data$mesor <- NA
    }
    if(!"amp" %in% vars){
      data$amp <- 1
    }
    if(!"phi" %in% vars){
      stop("data must contain 'phi' for PMC")   
    }
  }
  
  if(is.null(GrpID) | any(is.na(GrpID))){
    out <- pmc_internal(data, alpha)    
  }else{
    #browser()
    data$pr<-as.numeric(data$pr)
    data$mesor<-as.numeric(data$mesor)
    data$amp<-as.numeric(data$amp)
    data$phi<-as.numeric(data$phi)

    rownames<-col_concat(data[,GrpID], sep = ":")     #   uses assertr
    # split_factor <- get_split_factors(data, GrpID)
    # rownames <- names(split(data, split_factor)) 
    
    # loop over each group and bind results together
    out <- do.call('rbind',
                   lapply(split(data, rownames), function(x){
                     pmc_internal(x, alpha)
                   })
    )
    row.names(out)           #  <- unique(rownames)
  }
  
  

  
  if (Output$Doc){

    BaseTime<-Sys.time()        
    thisTime <- format(BaseTime, "--%d%b%y--%H-%M-%OS")
    #fileName2<-paste(fileName,thisTime,functionName,"PMC.txt",sep="")
    fileName3<-paste(fileName,thisTime,functionName,"PMC.rtf",sep="")
    
    rtf<-RTF(fileName3,width=11,height=8.5,omi=c(.5,.25,.5,.25), font.size=11)
    output<-gsub(pattern='\b\b',replacement='\b',fileName3)
    #  Parameters
    addParagraph(rtf,paste("Program CATkit.pmc --- Estimation of point  and interval estimates of population rhythm parameter"))
    addParagraph(rtf,paste(fileName3))
    addParagraph(rtf,paste("Probability level selected is ",alpha,"\n"))
    
    addParagraph(rtf,paste(title))
    #addParagraph(rtf,paste(functionName))
    
    addParagraph(rtf,paste("\n ------------ Rhythmometric Summary ------------\n"))
    
    addTable(rtf,out, col.justify='R',header.col.justify='R' ,font.size=11,row.names=TRUE,NA.string="--")
  }

  addNewLine(rtf,4)   
  done(rtf)
  opar = par()
  closeOutput(file=fileName3,output=Output,opar=opar)
  return(out)
}
