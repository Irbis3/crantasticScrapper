Actogram <-
function (endX, Avg=1, lum=4, yLab,modulo=1440, Rverbose=0,tz,CatBins ,fileName,fileNum,startData) {
  #  starting column, ending column, Average or Individual, #minutes between points, # points to bin, # minutes/unit (usually day), data, dark onset

  #require(MASS)
  #  12960/15 = 864    864/48=18
  # total pts/newBin = total pts graphed/bins per day  = #days

  #bins ##CatBins<-CatBin(TimeCol=TimeCol,startX, endX, Avg, lum, sizePts, binPts,modulo, startData=1, myData) 
  # returns newMinPerBin,dataPts,binsPerDay,dayPts,dayCnt,animals, endData, DataList,plotCnt
  # use all data (startdata-1);  startData value is dark onset
  
  startX<-2    #  time is in column 1
  DataList<-CatBins$DataList
  TimeBins<-CatBins$TimeBins
  startData<-1
  
  if (Avg)  {   
    DataLen<-length(DataList[])}
  else {DataLen<-length(DataList[1,])}   #  row/column are reversed when returned from CatBin
  #TimeBins<-CatBins$TimeBins
  startDark <-as.character(format(as.POSIXct(TimeBins[startData], origin="1970-01-01",tz=tz),"%H:%M:%S"))    #  get index of first darkness value
  op<-par()
  thisTime <- format(Sys.time(), "%b %d, %Y - %H:%M:%S");
  
  #bins ##nameVector = paste(" ",names(myData[,startX:endX]),sep=" ")
  nameVector = CatBins$binNames[-1]
  endXrel<-endX-startX+1
  if (modulo==1440){
    xNames <- substr(TimeBins[1:CatBins$dayPts],12,16)
    timeUnits <- "Time (hh:mm)"
  } else {
    xNames <- c(1:CatBins$dayPts)
    timeUnits <- paste("Time (#",CatBins$newMinPerBin,"-min. bins per ",modulo,"-min. cycle)",sep="")}
  newDayCnt <- floor(CatBins$dataPts/(modulo/CatBins$sizePts))
  print(newDayCnt)
  DataListSum<-vector(length=CatBins$dayPts)
  sumIndexes <-factor(rep_len(c(1:CatBins$dayPts),DataLen))

for (cnt in 1:CatBins$plotCnt) {      #  1 plot per animal, for all animals 
    par(new=FALSE)
    par(mfrow=c((newDayCnt+4+2),1),mar = c(0, 5, 0, 0),oma=c(1,1,6,1))    #debug specify height of each graph.  width?
    curX<-cnt+startX-1
    
    for (i in 1:(newDayCnt)) {     #  1 subplot per day for this animal (row)
      startPt <- ((i-1)*CatBins$dayPts)+1;   #  get data points for this day
      endPt <- i*CatBins$dayPts; 
      
      if (floor(i/2)!=(i/2)){   #  only if even
        yLabel<-substr(TimeBins[startPt],9,10)        #  2007-10-11  get day
      } else {yLabel<-""}
      if (i != newDayCnt){    
        #View(DataList[cnt,startPt:endPt])
        barplot(DataList[cnt,startPt:endPt],ylab=yLabel,cex.axis=.5,axes=FALSE)    #,ylab=endPt)
      } else {
        #barplot(DataList[cnt,startPt:endPt],ylab=yLabel,axes=FALSE)        # axis.lty = 1,
        barplot(DataList[cnt,startPt:endPt],ylab=yLabel,yaxt="n", axes=TRUE)
      }
      labelLoc = (max(DataList[cnt,startPt:endPt])-min(DataList[cnt,startPt:endPt])) *.4
    }  # end for (i in 1:newDayCnt)
    
    barplot(rep(0,length.out=CatBins$dayPts),col="white",col.axis="white",axes=FALSE)     #  this is a spacer between actogram and sum  (print box w/lines?)
    DataListSum[] <- by(DataList[cnt,], sumIndexes, sum)
    barplot(DataListSum[],axes=FALSE,ylab="Sum",cex.axis=.7, names.arg = xNames)       # substr(TimeBins[startPt:endPt],12,16)
    box("plot")
    #text(0,0, -20, labels = "Dark-->\n Onset", srt = 90, pos = 1, cex=.5,xpd = TRUE)
    text(0,0, labels = "-->              ", srt = 90, cex=.7,xpd = TRUE)
    
    if (Avg)  {    
      mtext(paste('Actogram\nFile',fileNum,', Avg across',CatBins$animals,'columns: ',nameVector[1],' - ',nameVector[endXrel],'\n',TimeBins[1],'  to  ',TimeBins[CatBins$endData]),outer=TRUE,side=3, line=1,cex =.9)  
    } else {
      mtext(paste('Actogram\nFile',fileNum,', Column',curX,': ',nameVector[cnt],'\n',TimeBins[1],'  to  ',TimeBins[CatBins$endData]),outer=TRUE,side=3, line=1,cex =.9)      
    }  # add file name, and # minutes per bin
    mtext("Date",outer=TRUE,side=3,line = 1, cex =.6, adj=.01)
    if (is.na(yLab)){yAxisLab<-nameVector[cnt]} else {yAxisLab==yLab}
    mtext(yAxisLab,outer=TRUE,side=2,line = -5, adj=.6)     #-1, -2 line worked  not 1
    #mtext(yAxisLab,outer=TRUE,side=2,line = 1, adj=.6) 
    mtext(timeUnits,side=1,line = 3, cex =.9)
    if (Rverbose!=-1){
    mtext(paste(CatBins$sizePts,'mins/point;   ',CatBins$binPts,'points/bin;   ',CatBins$newMinPerBin,'mins/bin;     ',CatBins$binsPerDay,'bins/day'),side=1,line=5,cex=.7) 
    }
    if (is.na(lum)){
      mtext("Starting with first data point.",side=1,line = 6.5, cex =.5, adj=.01)
    } else {
    mtext(paste("Dark onset:",startDark[1]),side=1,line = 6.5, cex =.5, adj=.01)
    }
    mtext(paste(fileName,"   ",thisTime),side=1,line = 6.5, cex =.5, adj=.8)
  par(fig=c(0,0.8,0,0.8), new=TRUE)    #creates a new plot

}  # end for (cnt in 1:CatBins$plotCnt)
#savePlot  -- saves to file a png?
  #remove read only list components 
  op$cin<-NULL
  op$cra<-NULL
  op$csi<-NULL
  op$cxy<-NULL
  op$din<-NULL
  op$page<-NULL
  par(op)
end
}
