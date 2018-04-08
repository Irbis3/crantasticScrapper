CatBin <-
function (TimeCol=1, startX, endX, Avg=1, lum=4, sizePts=2, binPts=15,modulo=1440, startData=1, myData) {
  #  this function accepts the matrix of Time and variables, and returns many values used in graphing results
  #  returns:  newMinPerBin,dataPts,dayPts,sizePts,binPts,dayCnt,binsPerDay,animals,endData, DataList,TimeBins,binNames
  #startX:  starting Column
  #endX:    ending Column
  #
  #Avg:  If True, an average of all Y columns selected will be done first, followed by individual  analyses of each Y column
  #      If False, the average is omitted.
  #
  #lum:  The column number containing luminance values or NA.  Luminance values are used to determine where the light level drops sharply, 
  #        and this point is used as the starting point for analysis.  Data points prior to this row (LumStart) are not used.
  #        The startX to endX can encompass this column.
  #
  #sizePts, binPts:  sizePts is the number of minutes between samples.  binPts is the number of samples to aggregate into one bin. 
  #      Binning is very flexible since it can be so important.  sizePts * binPts = number of minutes in each bin.   Only full bins are 
  #     used for analysis, so there could be a few data points at the end of the data (after binEnd) that are not used.
  #
  #modulo:  Only the Actogram function uses this parameter.   It specifies the width in minutes to be used for displaying the 
  #        Actogram.  Default is 1440 min, or 1 day.  
  #
  #startData:  Row at which data should start in myData
  #myData:  Data matrix
  #  if myData has 337 data;  and startData=27;  modulo=1440;  sizePts=30;  binPts=1; then dataPts=288;  moduloPts=48; endData=315
  
  #if (length(myData[,TimeCol])<3*modulo)
  #  print(paste(binPts,"There must be at least 3 days of data"))    #   stop

  newMinPerBin <- binPts*sizePts    
  if (floor(modulo/(newMinPerBin))!=(modulo/newMinPerBin)){
    #  bin size has to be a multiple of modulo so days can be split evenly
    stop(paste(binPts," is not a valid binning size as binPts*sizePts does not divide equally into modulo=",modulo))
  }
  moduloPts<-modulo/sizePts
  dataPts<-(moduloPts)*floor((length(myData[,endX])-startData+1)/moduloPts);  # #data pts that gives FULL bins only
  endData <- dataPts+startData-1    #  this is the row count of FULL myData vector, including some data that may not be used;  dataPts is row count of data to be USED
  dayPts <- modulo/newMinPerBin;    # number of dataPts in a day after binning happens
  dayCnt <- floor(dataPts/(modulo/newMinPerBin))   # number of days of dayPts in dataPts  --after binning
  dataCnt <- floor(dataPts/(dayPts*binPts));   # rnd toward 0:  dataPts w/ full bins;

  #cat("dataPts",dataPts,"dayPts",dayPts,"binPts",binPts,"dataCnt",dataCnt,"\n")
  #t<-1:1:dayPts;
  binsPerDay = modulo/newMinPerBin

  if (dataCnt<3){
    #  need a minimum of 3 days of data
    print(paste(dataPts," There are less than the required minimum of 3 days of data: only ",dataCnt))
  }
  animals <- endX-startX+1;
  DataBins <- matrix(data = NA, nrow = dataPts/binPts, ncol = (animals), byrow = FALSE, dimnames = NULL)
  #create an array to hold binned data  -- only as long as new bins
  
  if (binPts>0) {
    sumIndexes <-factor(rep(1:dataPts, each= binPts, length = dataPts))
    for (s in startX:endX){
      DataBins[,(s-startX+1)] <- by(myData[startData:endData,s], sumIndexes, sum)
    }
  } else {
    DataBins[1:dataPts,1:animals] <- myData[startData:endData,startX:endX];   #  no need to bin  -- just use data points;  untested with 1:....
  }
newEndData<-dataPts/binPts
  DataList <- vector(length=animals,mode="numeric")
  TimeBins <- vector(length=newEndData,mode="character")
  TimeBins2 <- vector(length=newEndData,mode="character")
  
  if (Avg)  {    
    DataList <- t(rowSums(DataBins[,1:animals]))/animals;  #  returns a row
    plotCnt <- 1;
  } else {
    DataList <- t(DataBins[,1:animals]);
    plotCnt <- animals;
  }

  
  TimeBins2<-as.character(format(as.POSIXct(myData[startData:endData,TimeCol], origin="1970-01-01",tz="GMT"),"%Y/%m/%d %H:%M:%S"))
  TimeBins<-TimeBins2[seq(1,endData-startData+1, by=binPts)]    # endData-startData+1 = length(sumIndexes)

  if (TRUE==FALSE){
    cat("newMinPerBin",newMinPerBin,"dataPts",dataPts,"dayPts",dayPts,"dayCnt\n",dayCnt,"binsPerDay",binsPerDay,"animals",animals,"endData",endData,"plotCnt\n",plotCnt)
  }
  return(list(newMinPerBin=newMinPerBin,dataPts=dataPts,dayPts=dayPts,sizePts=sizePts,binPts=binPts,dayCnt=dayCnt,binsPerDay=binsPerDay,animals=animals,endData=newEndData, DataList=DataList,TimeBins=TimeBins,binNames=names(myData[,1:endX]),plotCnt=plotCnt))
}
