CatCall <-
function (TimeCol=1, timeFormat="%Y%m%d%H%M",lum=4, valCols=c(3,4), sumCols=c(5,6), Avg=FALSE, export=FALSE, sizePts=2, binPts=5, Interval = 0, Increment=0, k=6, yLab="Activity Level (au)", modulo=1440,Rverbose=0, RmaxGap=400, Skip=0,header=FALSE, Smoothing=FALSE, Actogram=FALSE,AutoCorr=FALSE,CrossCorr=FALSE,Console=FALSE,Graphics="pdf", Darkness=1,LagPcnt=.33,tz="GMT",fileName,file2=list(Name=NULL,TimeCol=1, timeFormat="%Y%m%d%H%M", lum=4, valCols=c(3,4), sumCols=c(5,6),sizePts=2, binPts=5,Darkness=0)) {   #  ,Multitaper=F
  # at this point, lum works for both files, file2$lum is not yet working
  #          also, the periodogram turns out with highest precision if a multiple of a full period is used.
  #tz accepts any time zone to apply to data read.  GMT is the default, and most reliable usage.  There is usually no need to change it. 
  # documentation:  update web;  copy to here;  shift-cmd-C to Comment lines;  then change Help files similarly
  # -------------------------
  #
  # TimeCol:  Specify 2 time columns c(1,2) if date is in one and time is in another.  Specify one column (a scalar) if date time is all in one column
  #         
  # timeFormat:  specify the time format used in the data file:  "%Y%m%d%H%M" or "%d/%m/%y %H:%M:%S" or any allowed by R.  See strptime {base}
  #
  #lum:  The column number containing luminance values, or NA.  Luminance values are used to determine where the light level drops sharply, 
  #        and this point is used as the starting point for analysis.  Data points prior to this row (LumStart) are not used.
  #        The startX to endX can encompass this column.
  #
  #sumCols:  Columns that are counts, such that if you were to bin them, you would add them up;
  #
  #valCols:  Columns that are not counts, such that if you were to bin them, you would average them
  #
  #Avg:  If True, an average of all Y columns selected will be done first, followed by individual  analyses of each Y column
  #      If False, the average is omitted.
  
  #export:  Boolean.  Default is False.  If True, a data file is saved of the input data after filling in missing values and binning 
  #         (per parameters).  When True, each function (except Actogram) exports function results to separate comma delimited text files.
  
  #sizePts, binPts:  sizePts is the number of minutes between samples.  binPts is the number of samples to aggregate into one bin. 
  #      Binning is very flexible since it can be so important.  sizePts * binPts = number of minutes in each bin.   Only full bins are 
  #     used for analysis, so there could be a few data points at the end of the data (after binEnd) that are not used.
  
  #Interval, Increment:  These two parameters are used together to specify a progressive analysis.  The Interval is the length of subsections of
  #     data to analyze, and the increment is how far to move ahead in the data to begin the next Interval.  The entire data set will 
  #     be analyzed (from LumStart to binEnd).   A progressive analysis, as indicated by Table 2, is performed by the Auto-Correlation, 
  #     Cross-Correlation and Multitaper/Periodogram analyses.  The Actogram and Smoothing functions are performed on the full dataset 
  #     length, for each column, as normal.  There is no benefit to viewing these graphs in subsections.
  #     Interval will need to be large enough not to trigger the error messages in the functions, which require more than 3 days for each analysis
  
  #k:  Only the Smoothing function uses this parameter.  It is a count of the number of data points on each side of a point to 
  #        include in the moving average.  Each moving average is calculated using 2k data points.
  
  #yLab:  Label for Y axis on Smoothing and Actogram;  column headers in data file override this setting
  
  #modulo:  Only the Actogram function uses this parameter.   It specifies the width in minutes to be used for displaying the 
  #        Actogram.  Default is 1440 min, or 1 day.  
  
  # header:  uses the header information for labelling of graphs, if a header exists.  Also omits any data rows where the first character 
  #          of the data row matches the first character of the header.
  
  #Rverbose, RmaxGap:  Rverbose is for debug and can take on values of 0,1 or 2.  0 turns off debug information.  1 or 2 add 
  #increasing amounts of debug information.  -1 displays minimal information on graphs.   RmaxGap specifies the maximum allowable 
  #number of missing data points in any one block.   An error will be returned if gaps larger than this are found in either data file.
  
  #Skip, nFiles:  These are kind of housekeeping parameters.  
  #tz accepts any time zone to apply to data read.  GMT is the default, and most reliable usage.  There is usually no need to change it.  
  #Skip is a parameter used in the call to read.table function.  In case the header has multiple rows,some can be skipped.  The first row will be 
  #       read as the header (if header=T) and skip indicates how many rows to skip before reading data.  
  #nFiles can be set to 1 or 2.  Normally you will read in 2 files, the first containing baseline data and the second containing 
  #       experimental data, but it is also possible to specify only 1 file, in which case the Cross-Correlation function is not performed.
  
  #(Function):  For any function in Table 2, specifying (Function)=TRUE will cause it to run, thus you select only the functions you 
  #need for any purpose.  For example:  Actogram=TRUE, Smoothing=FALSE, AutoCorr=F, CrossCorr=T, ...
  # for CrossCorr, 2 files must be specified.  And for each file, the same number of columns must be specified -- if 2 columns are selected for file 1, the same number must be selected for file2
  
  #Console:  Default is F.  When Console=T output will be redirected to the RStudio Console, instead of an output file.
  
  #Graphics:  Results of CatCall are sent to a files when Console=F.  Default file output type is "pdf".  Possible values: "jpg, pdf, tif, png.   TIFF and PNG are higher resolution than jpeg and pdf.

  #Plots in PNG and JPEG format can easily be converted to many other bitmap formats, and both can be displayed in modern web browsers. 
  #The PNG format is lossless and is best for line diagrams and blocks of colour. The JPEG format is lossy, but may be useful for image 
  #plots, for example. It is most often used in html web pages.  TIFF is a meta-format: the default format written by tiff is lossless 
  #and stores RGB (and alpha where appropriate) values uncompressed-such files are widely accepted, which is their main virtue over PNG.
  
  #Darkness1, Darkness2:  This refers to the illumination column.  CAT analysis and graphing begins at darkness onset, as indicated by 
  #the lumninance column.  Normally, darkness is indicated by a very small number (<10) and light is a large number (>=10).  If this is 
  #needed to be reversed, changing the Darkness 1 or Darkness2 defaults will correct the interpretation of the lumninance column for 
  #file1 or file2, respectively.  Darknessx=0 means that darkness is a small number (<10).
  
  #LagPcnt:  Specifies the percent of the data set to use in the calculation for Autocorrelation and Crosscorrelation.
  #           Should be designated as a fraction, to get the highest precision:  1/3
  #           The resulting length used is rounded to the nearest modulo, for best result.  rnd((CatBins$dataPts/modulo)*LagPcnt)
  #
  #   Input Data:  
  #     Input data is assumed to be equidistant, discrete, except for the luminance column, and the adjacent column to the left of 
  #         lumninance (which may be temperature, but is not currently processed in CAT).  (Although CAT can be adapted easily to accept 
  #         a parameter that will allow continuous data.)  An error will be reported if less than three days of data is provided in a file.  
  #         All columns are expected to be numeric.  
  #   
  #   Data File format:  Tab delimited (.txt) file with the following columns:  
  #     
  #     Date Time V0 luminance V1  V2  V3  V4  ...
  #   
  #   Output Data:
  #     Sample graphics output file:  See Output section on the web site for a sample of a full output file.  All graphs have the input 
  #          data filename to clearly identify the data file under analysis, and a timestamp to show the time of analysis.  Each graph also 
  #          lists the column name being analyzed (or averaged),  and the starting and ending times of analysis, as they vary slightly from 
  #          the full data set (Lum to binEnd).    
  #   Possible values:  jpg, pdf, tif, png
  #   
  #   Output Data files:  The input data is interpolated and binned.  This transformed data can be exported using the export parameter.  
  #           If export=T then each function (except Actogram) exports a file with the results of the function.
  #   
  # ------------------------------
#   initial.dir<-getwd()
#   # change to the new directory
#   setwd('~/Documents/Cathy/Neuroscience papers/capstone/Bartolomucci/chrono data/R-functions')
#   #funprog
#   source('~/Documents/Cathy/Neuroscience papers/capstone/Bartolomucci/chrono data/R-functions/CatBin/CatBin.R')
#   source('Smooth/Smooth.r')
#   source('AutoCorr/AutoCorr.r')
#   #source('AutoCorrRand.r')
#   source('Actogram/Actogram.r')
#   source('CrossCorr/CrossCorr.R')
#   #source('CrossCorrX.r')
#   source('MultiTaper/MultiTaper.v4.r')     #  2 periodograms, higher for use in paper
#   source('MultiTaper/MultiTaper.v2.r')     #  1 periodograms, 1 Ftest  (adjust text x length)
#   #source('~/Documents/Cathy/Neuroscience papers/capstone/Chronobiology/CosinorR/periCosinor.R')
#   source('Wavelet/Wavelet.r') 
#   source('Import/Import5.R')
#   #source('~/Documents/Cathy/Neuroscience papers/capstone/Chronobiology/CosinorR/CosinorEQ.r')
  #print(startX,endX)
  TimeColParam<-TimeCol
  valColsParam<-valCols
  sumColsParam<-sumCols
  lumParam<-lum
TimeColParam2<-file2$TimeCol
valColsParam2<-file2$valCols
sumColsParam2<-file2$sumCols
#lumParam<-lum
  RverboseParam<-Rverbose

# read data;  if no data file name given, prompt the user for the filename
if (length(fileName)==0 || !exists("fileName")){
  print("Enter a baseline data file (tab delimited):")
  fileName <- file.choose()
}
  MyDataDelimiter <- read.table(fileName, nrows=30, header=header, sep="\t",stringsAsFactors=FALSE,skip=Skip)          
  #  only read a few rows to determine if CSV or tab delimited
  if (length(MyDataDelimiter[5,])>1){   #  if count >1, this is tab delimited
    delim1<-'\t'
  } else {delim1<-','}
  #  read the header
  myHeader <- read.table(fileName, header=TRUE, nrows=1, sep=delim1,skip=Skip)
  #get the header for names  -- all strings matching header will be removed
  if (header){
    headerChar1 <- substr(names(myHeader)[1],1,1)    # gets the 1st character of the header to be used as a comment indicator
  } else headerChar1 <- ""               #  use NA as a comment indicator
  myData1A <- read.table(fileName, header=FALSE, sep=delim1,skip=Skip,stringsAsFactors=FALSE,comment.char=headerChar1,col.names = names(myHeader))
  # header=FALSE because the header is stripped by comment.char!!!
  if (.Platform$file.sep=="/"){
    m=regexec("[^/]*$",fileName)            #     mac=[^/]*$
  } else {
    m=regexec("[^\\]*$",fileName)           #./(.$)    pc=[^\\]*$
  }
  fileName1<-regmatches(fileName,m)
  print(fileName1)          #   (for use in graph)

  if (anyDuplicated(c(valCols,sumCols))){stop("sumCol and valCol parameter vectors must be mutually exclusive.")}
  if (all(is.na(c(valCols,sumCols)))){stop("sumCol and valCol parameter vectors are both empty.  At least one column must be specified.")}
  TimeColCnt<-length(TimeCol)
  valColCnt<-length(valCols)
  sumColCnt<-length(sumCols)

  if (is.na(lum)){
    myData1 = myData1A[,c(TimeCol,valCols,sumCols)]
  } else {       # removed na.omit()
    myData1 = myData1A[,c(TimeCol,valCols,sumCols,lum)]
    lum<-length(myData1[1,])
  }

  if (valColCnt>0) {valCols<-c(1:valColCnt) +1}            #  calculate new columns for valCols
  if (sumColCnt>0) {sumCols<-c(1:sumColCnt) +valColCnt+1}   #  calculate new columns for sumCols 
  Y1<-c(sumCols,valCols)
  if (TimeColCnt==2){    #  append 2 columns
    TimeCol<-c(1,2)
    myData1[,TimeCol[1]] <- as.POSIXct(strptime(x=paste(myData1[,TimeCol[1]],myData1[,TimeCol[2]]), format=timeFormat, tz=tz))
    myData1[,TimeCol[2]] <- NULL

    TimeCol<-1
    if (!is.na(lum)) {lum<-length(myData1[1,])}     # column number of lum changes because one column is gone
  } else if (TimeColCnt == 1) {  #  if not in 2 columns, use only column specifiednn
    TimeCol<-1
    myData1[,TimeCol] <- as.POSIXct(strptime(x=myData1[,TimeCol], format=timeFormat, tz=tz))
  }
  colnames(myData1)[TimeCol] <- "Datetime"
  str(myData1)
  if (Rverbose == -1){
    importVerbose<-2
  } else {importVerbose<-Rverbose}
if (is.na(myData1[2,TimeCol])){
  print("Check your time format -- it is likely not matching your actual time/date.  Time column(s) are not being read properly")
}
  print("passing to import")
#browser()
  myList <- do.call(Import, list(x=myData1, tsCol=TimeCol, valCols=na.omit(c(valCols,lum)), sumCols=sumCols, rowDurationSeconds=sizePts*60, maxGap=RmaxGap, verbose=importVerbose,tz=tz))

  print(paste("Max gap baseline = ", myList$maxGap))
  print(paste("Percent data missing = ", myList$percentMissing))#  if error exit:
  if (!myList$errorExit){    #  import did not return with an error.
    baseData<-myList$df

  nFiles<-1
  if (CrossCorr==TRUE){
    nFiles<-2
  }

  if (nFiles==2){
      # read data;  if no data file name given, prompt the user for the filename
      if (length(file2$Name)==0){
        print("An experimental data file (tab delimited) is needed for Crosscorrelation:")
        file2$Name <- file.choose()
      }
      MyDataDelimiter <- read.table(file2$Name, nrows=30, header=header, sep="\t",stringsAsFactors=FALSE,skip=Skip)          
      #  only read a few rows to determine if CSV or tab delimited
      if (length(MyDataDelimiter[5,])>1){   #  if count >1, this is tab delimited
        delim2<-'\t'
      } else {delim2<-','}
      #  read the header
      myHeader <- read.table(file2$Name, header=TRUE, nrows=1, sep=delim2,skip=Skip)
    
      #get the header for names  -- all strings matching header will be removed
      if (header){
        headerChar1 <- substr(names(myHeader)[1],1,1)    # gets the 1st character of the header to be used as a comment indicator
      }
      myDataA <- read.table(file2$Name, header=FALSE, sep=delim2,skip=Skip,stringsAsFactors=FALSE,comment.char=headerChar1,col.names = names(myHeader))
      
      #m=regexec("[^/]*$",fileName)            #  ./(.$)   [^\\/]*\s   mac=[^/]*$    [^\\]*$ 
      if (.Platform$file.sep=="/"){
        m=regexec("[^/]*$",file2$Name)            #  ./(.$)    pc=[^\\]*$   mac=[^/]*$
      } else {
        m=regexec("[^\\]*$",file2$Name)
      }
      file2Name1<-regmatches(file2$Name,m)
      print(file2Name1)    # get rid of : from timestamp (for use in graph)
      
      if (anyDuplicated(c(file2$valCols,file2$sumCols))){stop("File2 sumCol and valCol parameter vectors must be mutually exclusive.")}
      if (all(is.na(c(file2$valCols,file2$sumCols)))){stop("File2 sumCol and valCol parameter vectors are both empty.  At least one column must be specified.")}
      
      TimeColCnt2<-length(TimeColParam2)
      valColCnt2<-length(valColsParam2)
      sumColCnt2<-length(sumColsParam2)

      if (is.na(lum)){
        myData = myDataA[,c(file2$TimeCol,file2$valCols,file2$sumCols)]       #  removed na.head(xxx,n=-1)
      } else {     #  removed na.omit()
        myData = myDataA[,c(file2$TimeCol,file2$valCols,file2$sumCols,lum)] 
      }
      
      #file2$valCols<-file2$valCols-file2$valCols[1]+2             #  calculate new columns for valCols
      #file2$sumCols<-file2$sumCols-file2$sumCols[1]+valColCnt2+2    #  calculate new columns for sumCols
      if (valColCnt2>0) {file2$valCols<-c(1:valColCnt2) +1}            #  calculate new columns for valCols
      if (sumColCnt2>0) {file2$sumCols<-c(1:sumColCnt2) +valColCnt2+1}   #  calculate new columns for sumCols 
      
      Y2<-c(file2$sumCols,file2$valCols)
      if (TimeColCnt2==2){    #  append 2 columns
        myData[,1] <- as.POSIXct(strptime(x=paste(myData[,1],myData[,2]), format=file2$timeFormat, tz=tz))
        myData[,2] <- NULL

        TimeCol2<-1
        if (!is.na(lum)) {lum<-length(myData[1,])}     # column number of lum changes because one column is gone
      } else if (TimeColCnt2 == 1) {  #  use only column specified
        TimeCol2<-1
        myData[,TimeCol2] <- as.POSIXct(strptime(x=myData[,TimeCol2], format=file2$timeFormat, tz=tz))
      }
      colnames(myData)[TimeCol2] <- "Datetime"
      str(myData)

      myList2 <- do.call(Import, list(x=myData, tsCol=TimeCol2, valCols=na.omit(c(file2$valCols,lum)), sumCols=file2$sumCols, rowDurationSeconds=file2$sizePts*60, maxGap=RmaxGap, verbose=importVerbose,tz=tz))
      
      print(paste("Max gap experiment = ", myList2$maxGap))
      print(paste("Percent data missing = ", myList2$percentMissing))
      
      #if (is.data.frame(myList$df)){
      #  View(myList$df)
      #}
      xData<-myList2$df
  } else {file2Name1<-""; file2$Name<-""}
  thisTime <- format(Sys.time(), "--%d%b%Y--%H-%M-%S")
  fileName3 <-paste(fileName,thisTime,"CAToutput.rtf",sep="")
  #docName <- sub("-","",sub("-","",fileName,fixed=TRUE),fixed=TRUE)
  if (!Console){
    if (Graphics=="pdf"){
      fileName4 <-paste(fileName,thisTime,"CAToutput.pdf",sep="")
      pdf(file=fileName4,width=8, height=10)                #,width=8, height=10)
      } else {if (Graphics=="jpg"){
          jpeg(filename=fileName4,width=8, height=10)  
          fileName4 <-paste(fileName,thisTime,"CAToutput.jpg",sep="")#,width=8, height=10)
        } else {if (Graphics=="png"){
            png(filename=fileName4,width=8, height=10) 
            fileName4 <-paste(fileName,thisTime,"CAToutput.png",sep="")#,width=8, height=10)
          } else {if (Graphics=="tiff"){
              tiff(filename=fileName4,width=8, height=10)                #,width=8, height=10)
              fileName4 <-paste(fileName,thisTime,"CAToutput.tif",sep="")
            }} 
        } }}
  
  percentMissing<-myList$percentMissing
  percentMissing2<-NA
  maxGap<-myList$maxGap
  maxGap2<-NA
  if (export==T){
    file1out<-paste(fileName,thisTime,"CATfile1binned.txt",sep="")
    file2out<-""
    file1outSmooth<-paste(fileName,thisTime,"CATfile1-fSmooth.txt",sep="")
    file2outSmooth<-""
    file1outCorr<-paste(fileName,thisTime,"CATfile1-fCorr.txt",sep="")
    file2outCorr<-""
    #file1outXcorr<-paste(fileName,thisTime,"CATfile1-fXcorr.txt",sep="")   # XCorr is not run if only one file
    fileoutXcorr<-""
    if (nFiles==2){
      file2out<-paste(fileName,thisTime,"CATfile2binned.txt",sep="")
      file2outSmooth<-paste(fileName,thisTime,"CATfile2-fSmooth.txt",sep="")
      file2outCorr<-paste(fileName,thisTime,"CATfile2-fCorr.txt",sep="")
      fileoutXcorr<-paste(fileName,thisTime,"CATfile-fXcorr.txt",sep="")
      percentMissing2<-myList2$percentMissing
      maxGap2<-myList2$maxGap
    }
  } else {file1out<-file1outSmooth<-file1outCorr<-file1outXcorr<-""
          file2out<-file2outSmooth<-file2outCorr<-fileoutXcorr<-""
  }

CatCover(TimeCol=TimeColParam, TimeFormat=timeFormat, valCols=valColsParam, sumCols=sumColsParam, lum=lumParam, Avg=Avg, export=export, percentMissing=c(percentMissing, percentMissing2),  maxGap<-c(maxGap,maxGap2), sizePts=sizePts, binPts=binPts, Span = Interval, Increment=Increment, k=k, yLab=yLab, modulo=modulo,header=header, Rverbose=Rverbose, RmaxGap=RmaxGap, tz=tz,Skip=Skip,nFiles=nFiles, Smoothing=Smoothing, Actogram=Actogram,AutoCorr=AutoCorr,CrossCorr=CrossCorr,Console=Console,Graphics=Graphics,Darkness1=Darkness,Darkness2=file2$Darkness,LagPcnt=LagPcnt,File1=fileName,File2=file2$Name,File1out=file1out,File2out=file2out)   # Multitaper=Multitaper,
  print("output fileName4")
  print(fileName4)

  if (FALSE) {  
  print("cosinor path") 
  #CosinorEQ(startX, endX, Avg=0, lum, sizePts, binPts, period=24, myData = baseData,fileName1)
  #Cosinor(TimeCol=2,Y=c(startX,endX), Units='Week', RegressionLen=1, RefDate="201302030000", IntervalUnits='Day', Span=Interval, Increment=Increment, header=F,oneCycle=0, Debug=FALSE,call=TRUE,data=MyData1,file=fileName1)
  #Cosinor(TimeCol=2,Y=c(startX,endX), Units='Week', RegressionLen=1, RefDate="201302030000", IntervalUnits='Day', Span=Interval, Increment=Increment, header=F,oneCycle=0, Debug=FALSE,call=TRUE,data=MyData,file=file2Name1)  
  } else{
  baseData_len<-length(baseData[,1])         # #####################################
  endX<-length(baseData[1,])
  startX<-2
  if (nFiles==2){
    endX2<-length(xData[1,])
    if (endX!=endX2){
      stop("File1 and File2 must have the same number of data columns, but do not.")
    }
}
  if (is.na(lum) && (!is.na(Darkness) || !is.na(file2$Darkness))){
      print("lum=NA, therefore Darkness1 and Darkness2 are assumed to be NA")
      Darkness<-NA
      file2$Darkness<-NA
  } else if (!is.na(lum) && (is.na(Darkness) || is.na(file2$Darkness))){
      print("Darkness parameter set to NA, lum parameter is assumed to be NA")
      Darkness<-NA
      file2$Darkness<-NA
    }
  #View("Enter a baseline data file (tab delimited):")

  nextLight<-0
  if (is.na(Darkness)){
    nextLight<-1     #Lyazzat  was 10
    startDataV<-1
  } else {
    LumV <- baseData[,lum]         #   get just the luminance vector
      if (LumV[1]==0 && Darkness==0){             #  if the first row is darkness, more into light first
        if (all(LumV==0)) {stop("All luminance values are 0 (Darkness1=0).  No light onset found.")}
        nextLight<-which(LumV>0)     #Lyazzat  was 10
        LumV<-baseData[nextLight[1]:baseData_len,lum]
      }
      if (Darkness!=0){
        if (all(LumV<=10)) {stop("All luminance values are <=10 (Darkness1!=0).  No dark onset found.")}
        Dark <- LumV[LumV>10]       #   get values for darkness
      } else {
        if (all(LumV>.3)) {stop("All luminance values are >.3 (Darkness1=0).  No dark onset found.")}
        Dark <- LumV[LumV<=.3]}    #Lyazzat  was 10
    startDataV <- which(LumV==Dark[1])   + nextLight[1]    #  get index of first darkness value
  }
  startData <-startDataV[1]
  cat("darkness1",startData,"\n")

  if (nFiles==2){
    xData_len<-length(xData[,1])         # #############################################
    nextLight<-0
    if (is.na(file2$Darkness)){
      nextLight<-1     #Lyazzat  was 10
      startDataV<-1
    } else {
      LumV <- xData[,lum]         #   get just the luminance vector
        if (LumV[1]==0 && file2$Darkness==0){             #  if the first row is darkness, more into light first
          if (all(LumV==0)) {stop("All luminance values are 0 (Darkness2=0).  No light onset found.")}
          nextLight<-which(LumV>0)     #Lyazzat  was 10
          LumV<-xData[nextLight[1]:xData_len,lum]
        }
        if (file2$Darkness!=0){
          if (all(LumV<=10)) {stop("All luminance values are <=10 (Darkness2!=0).  No dark onset found.")}
          Dark <- LumV[LumV>10]       #   get values for darkness
        } else {
          if (all(LumV>.3)) {stop("All luminance values are >.3 (Darkness2=0).  No dark onset found.")}
          Dark <- LumV[LumV<=.3]}    #Lyazzat  was 10
      startDataV <- which(LumV==Dark[1])  + nextLight[1]       #  get index of first darkness value
    }
    startDataX <-startDataV[1]
    cat("darkness2",startDataX,"\n")
  }
#browser()
  CatBins1<-CatBin(TimeCol=TimeCol, startX, endX, Avg=0, lum, sizePts, binPts,modulo, startData,baseData)
  
  if (export==T){
    # returns newMinPerBin,dataPts,binsPerDay,animals, startData, endData, DataList,plotCnt
    #write.matrix(cbind(CatBins1$TimeBins,t(CatBins1$DataList)), file = file1out, sep = delim1)  #  no headers
    backup1<-baseData
    baseData<-cbind(CatBins1$TimeBins,t(CatBins1$DataList))
    colnames(baseData)<-t(CatBins1$binNames)
    write.matrix(baseData, file = file1out, sep = delim1)    # headers
    if (nFiles==2){
      CatBins2<-CatBin(TimeCol=TimeCol2, startX, endX2, Avg=0, lum, file2$sizePts, file2$binPts,modulo, startDataX,xData)
      # returns newMinPerBin,dataPts,binsPerDay,animals, startData, endData, DataList,plotCnt
      backup2<-xData
      xData<-cbind(CatBins2$TimeBins,t(CatBins2$DataList))
      colnames(xData)<-CatBins2$binNames
      write.matrix(xData, file = file2out, sep = delim2)
    }
  }

  if (Smoothing){
    print("smoothing")
    if (Avg) {Smooth(endX, Avg=1, lum, k,yLab,modulo=modulo, Rverbose=Rverbose,CatBins = CatBins1,fileName1,fileNum=1,startData,export,file1outSmooth) }
    Smooth(endX, Avg=0, lum, k,yLab,modulo=modulo, Rverbose=Rverbose,CatBins = CatBins1,fileName1,fileNum=1,startData, export,file1outSmooth)

     if (nFiles==2){
       if (Avg) {Smooth(endX2, Avg=1, lum, k,yLab,modulo=modulo,Rverbose=Rverbose,CatBins = CatBins2,file2Name1,fileNum=2,startDataX,export,file2outSmooth)}
        Smooth(endX2, Avg=0, lum, k,yLab,modulo=modulo,Rverbose=Rverbose,CatBins = CatBins2,file2Name1,fileNum=2,startDataX,export,file2outSmooth)
     }   # end of nFiles==2
  }      # end of Smoothing
    if (Actogram){
    print("actogram")
    if (Avg) {Actogram(endX, Avg=1, lum, yLab,modulo=modulo, Rverbose=Rverbose,tz=tz,CatBins = CatBins1,fileName1,fileNum=1,startData)} 
    Actogram(endX, Avg=0, lum, yLab,modulo=modulo, Rverbose=Rverbose,tz=tz,CatBins = CatBins1,fileName1,fileNum=1,startData)
     if (nFiles==2){
       if (Avg) {Actogram(endX2, Avg=1, lum, yLab,modulo=modulo, Rverbose=Rverbose,tz=tz,CatBins = CatBins2,file2Name1,fileNum=2,startData)}
        Actogram(endX2, Avg=0, lum, yLab,modulo=modulo, Rverbose=Rverbose,tz=tz,CatBins = CatBins2,file2Name1,fileNum=2,startData)
     }   # end of nFiles==2
   }     # end of Actogram
  Others<-TRUE
  if (Others){

    if (Interval<=0 || Increment<=0 || Increment*(1440/sizePts)>=baseData_len){
      baseIncrement<-0
      baseInterval<-baseData_len
      StartIntervals<-seq(from=startData, to=baseData_len, by=baseData_len)
    }
    else {
      # Interval or increment units are assumed to be in Days  X  
      baseInterval<-Interval*(1440/sizePts)      # #days  X  data points per day  (minutes in a day/ length between pts)  
      baseIncrement<-Increment*(1440/sizePts)      # #days  X  new bins per day
      StartIntervals<-seq(from=startData, to=(baseData_len-(baseIncrement-1)), by=baseIncrement)    #to=(baseData_len-(baseIncrement-1))
    }
    cat("Interval",baseInterval,"Increment",baseIncrement,"StartIntervals",StartIntervals,"baseData_len",baseData_len,"\n")
    baseProgression_end<-length(StartIntervals)
    
    #  move baseline autocorrelation and baseline multitaper to here    #######################################
    #browser()
    if (nFiles==2){
      
      if (Interval<=0 || Increment<=0 || Increment*(1440/file2$sizePts)>=xData_len){
        xIncrement<-0
        xInterval<-xData_len
        xStartIntervals<-seq(from=startDataX, to=xData_len, by=xData_len)
      } else {
        # Interval or increment units are assumed to be in Days  X  
        xInterval<-Interval*(1440/file2$sizePts)      # #days  X  data points per day  (minutes in a day/ length between pts)  
        xIncrement<-Increment*(1440/file2$sizePts)      # #days  X  new bins per day
        xStartIntervals<-seq(from=startDataX, to=xData_len-(xIncrement-1), by=xIncrement)     # to=xData_len-(xIncrement-1)
      }
      cat("xInterval",xInterval,"xIncrement",xIncrement,"xStartIntervals",xStartIntervals,"xData_len",xData_len)
      xProgression_end<-length(xStartIntervals)
   } else {xProgression_end<-0}
  
    if (xProgression_end>=baseProgression_end){
      Progression_end<-xProgression_end
    } else {
      Progression_end<-baseProgression_end
    }  
  baseSkip<-FALSE
  xSkip<-FALSE

    for (j in 1:Progression_end){   #  make a vector with incremental values.  use var(i) for necessary indexes

      if (xProgression_end>=j){
        xSkip<-FALSE
        xEndInterval<-xStartIntervals[j]+xInterval -1      #   Add Interval length to the last Interval to get the ending of the Interval
        if (xEndInterval>xData_len){      # *****what to do about this???**********
          xEndInterval<-xData_len
        }
      } else {                                 # if this increment is past end of Progression, skip
          xSkip <-TRUE
      }

      if (1==1) {  
        if (AutoCorr){
          print("AutoCorr")
          if (!baseSkip){
            if (Avg) {AutoCorr(endX, Avg=1, lum, modulo=modulo, Rverbose=Rverbose, CatBins = CatBins1,fileName=fileName1,fileNum=1,Span = Interval, Increment=Increment,LagPcnt=LagPcnt,export=export,fileoutCorr=file1outCorr)} 
            AutoCorr(endX, Avg=0, lum, modulo=modulo, Rverbose=Rverbose,CatBins = CatBins1,fileName=fileName1,fileNum=1,Span = Interval, Increment=Increment,LagPcnt=LagPcnt,export=export,fileoutCorr=file1outCorr)
          #  AutoCorr(startX, endX, Avg=0, lum, sizePts, binPts,Rverbose=Rverbose,myData = baseData[startData:baseData_len,],fileName=fileName1,Span = Interval, Increment=Increment,LagPcnt=LagPcnt,1,export,file1outCorr)
          }
          if (!xSkip & nFiles==2){
            if (Avg) {AutoCorr(endX2, Avg=1, lum, modulo=modulo, Rverbose=Rverbose,CatBins = CatBins2, fileName=file2Name1,fileNum=2,Span = Interval, Increment=Increment,xStartIntervals[j],xEndInterval,LagPcnt=LagPcnt,export=export,fileoutCorr=file2outCorr)} 
            AutoCorr(endX2, Avg=0, lum, modulo=modulo, Rverbose=Rverbose,CatBins = CatBins2, fileName=file2Name1,fileNum=2,Span = Interval, Increment=Increment,xStartIntervals[j],xEndInterval,LagPcnt=LagPcnt,export=export,fileoutCorr=file2outCorr)
          }  
        }  #end AutoCorr
        if (CrossCorr){
          print("CrossCorr")          
          if (nFiles!=2){stop("To run CrossCorr, be sure nFiles=2;  otherwise, set CrossCorr=F")}
          if (!xSkip & nFiles==2){
            #browser()
            #  have to pass in start of baseline relative to dark (or phase is off!)  -- test with progressive?           Only 1 output file!
            #  endX2 is not needed, as both files must have the same number of columns  -- xcorr is done col-by-col
            if (Avg) {CrossCorr(endX, Avg=1, lum, modulo=modulo, LagPcnt=LagPcnt,Rverbose=Rverbose,CatBins1 = CatBins1,CatBins2 = CatBins2,file2Name1,Span = Interval, Increment=Increment,xStartIntervals[j],xEndInterval,export=export,fileoutXcorr=fileoutXcorr)}
            CrossCorr(endX, Avg=0, lum, modulo=modulo, LagPcnt=LagPcnt,Rverbose=Rverbose,CatBins1 = CatBins1,CatBins2 = CatBins2,file2Name1,Span = Interval, Increment=Increment,xStartIntervals[j],xEndInterval,export=export,fileoutXcorr=fileoutXcorr)
          }
        }   # end CrossCorr
     }    #  end turning stuff off
#       if (Multitaper==T){
#         print("MultiTaper")
#           #View(baseData[StartIntervals[j]:EndInterval,1])
#         print(format(baseData[StartIntervals[j],1], "%b %d, %Y - %H:%M:%S"))
#         #print(format(baseData[EndInterval,1], "%b %d, %Y - %H:%M:%S"))
#         if (!xSkip){
#           print(format(xData[xStartIntervals[j],1], "%b %d, %Y - %H:%M:%S"))
#           print(format(xData[xEndInterval,1], "%b %d, %Y - %H:%M:%S"))
#        }
#         if (!baseSkip){
#           if (Avg) {MultiTaper.v4(TimeCol=TimeCol, startX, endX, Avg=1, lum=lum, sizePts, binPts,modulo=modulo, Rverbose=Rverbose,export=export,myData = baseData[startData:baseData_len,],fileName1,Span = Interval, Increment=Increment )}
#           MultiTaper.v4(TimeCol=TimeCol, startX, endX, Avg=0, lum-lum, sizePts, binPts,modulo=modulo, Rverbose=Rverbose,export=export,myData = baseData[startData:baseData_len,],fileName1,Span = Interval, Increment=Increment)
#         }
#         if (!xSkip & nFiles==2){
#           if (Avg) {MultiTaper.v4(TimeCol=TimeCol, startX, endX, Avg=1, lum=lum, sizePts, binPts,modulo=modulo, export=export,Rverbose=Rverbose,myData = xData[xStartIntervals[j]:xEndInterval,],file2Name1,Span = Interval, Increment=Increment)}
#           MultiTaper.v4(TimeCol=TimeCol, startX, endX, Avg=0, lum=lum, sizePts, binPts,modulo=modulo, export=export,Rverbose=Rverbose,myData = xData[xStartIntervals[j]:xEndInterval,],file2Name1,Span = Interval, Increment=Increment)
#         }
#       }
      }   #   end progressive
 }   # end if OTHERS
  #MultiTaper.fpeaks(startX, endX, Avg=0, lum, sizePts, binPts,myData = baseData,fileName1)
  
      #print("Wavelet")
      #Wavelet (TimeCol=TimeCol, startX, endX, Avg=1, lum, sizePts, binPts,myData = baseData,fileName1)
      #Wavelet (TimeCol=TimeCol, startX, endX, Avg=0, lum, sizePts, binPts,myData = baseData,fileName1)
      
      #Wavelet (TimeCol=TimeCol, startX, endX, Avg=1, lum, sizePts, binPts,myData = xData,file2Name1)
      #Wavelet (TimeCol=TimeCol, startX, endX, Avg=0, lum, sizePts, binPts,myData = xData,file2Name1)
  # change back to the original directory
  }  #  non-cosinor path    
  ###setwd(initial.dir)

  } # end if no error from import
  if (!Console){  
    dev.off()
    print(paste("Output has been saved to:",fileName4))
  } 
  # close the output file
  ##sink()
  # unload the libraries
  ##detach("package:nlme")

}
