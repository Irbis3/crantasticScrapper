CrossCorr <-
function (endX, Avg=1, lum=4, modulo=1440, LagPcnt=.33,Rverbose=0, CatBins1,CatBins2,fileName,Span, Increment,xStartSpans=1,xEndSpan,export=F,fileoutXcorr="") {
  #  starting column, ending column, Average or Individual, #minutes between points, # points to bin, # minutes/unit (usually day)
  # require delftfews, chron, zoo?
  #require(delftfews)
  #modulo = 1440
  startData1 <-1
  startData2 <-1
  
  #bins ##CatBins1<-CatBin(TimeCol=TimeCol,startX, endX, Avg, lum, sizePts, binPts,modulo, startData=1, myData1) 
  #bins ##CatBins2<-CatBin(TimeCol=TimeCol,startX, endX, Avg, lum, sizePts, binPts,modulo, startData=1, myData2) 
  # returns newMinPerBin,dataPts,binsPerDay,dayPts,dayCnt,animals, endData, DataList,plotCnt
  
  DataList1<-CatBins1$DataList
  DataList2<-CatBins2$DataList
  startX<-2    #  time is in column 1
  TimeBins1<-CatBins1$TimeBins
  TimeBins2<-CatBins2$TimeBins
  startDark <-as.character(format(as.POSIXct(TimeBins1[startData1], origin="1970-01-01",tz="GMT"),"%H:%M:%S"))    #  get index of first darkness value
  op<-par()
  thisTime <- format(Sys.time(), "%b %d, %Y - %H:%M:%S");
  if (Avg){
    fileoutXcorrX <- sub(pattern=".", replacement="-Avg.", x=fileoutXcorr, fixed = TRUE)   # name of file for exported data
  }  # otherwise use name as passed

  #bins ##nameVector <- paste(" ",names(myData1[,startX:endX]),sep=" ")
  nameVector1 = CatBins1$binNames[-1]     # exclude the DateTime header name
  endXrel<-endX-startX+1
  #bins ##nameVector2 <- paste(" ",names(myData2[,startX:endX]),sep=" ")
  nameVector2 = CatBins2$binNames[-1]     # exclude the DateTime header name
  endXrel<-endX-startX+1
  
  for (cnt in 1:CatBins1$plotCnt) {      #  1 plot per animal, for all animals 
    par(mfrow=c(2,1), mar = c(4, 4, 4, 0),oma=c(2,0,1,1)) 
    curX<-cnt+startX-1
    #MaxLag <- (CatBins1$dataPts/CatBins1$binPts)*.33
    #MaxLag <- rnd((CatBins$dataPts/modulo)*LagPcnt)
    MaxLag <- round((CatBins1$dataPts/CatBins1$dayPts)*LagPcnt)*CatBins1$dayPts
    #  MaxLag <- ceiling((CatBins$dataPts/CatBins$binPts)*LagPcnt)
    getCCF <- ccf(DataList1[cnt,],DataList2[cnt,],main="",type="correlation",ylab="Correlation coefficient (r)",lag.max = MaxLag,xlab = "",panel.first = abline(v=seq(from=-MaxLag, to=MaxLag, by=(CatBins1$binsPerDay)),xaxt="n",col="darkslategray",lty="dotted"))
    # Default is 10*log10(N/m) where N is the number of observations and m the number of series. 
    # Will be automatically limited to one less than the number of observations in the series.

    maxCheck <- CatBins1$dayPts/2
    periodPt <- which.max(getCCF$acf[]) 
    #periodPt <- which.max(getACF$acf[maxCheck:(CatBins$dayPts+maxCheck)]) + maxCheck - 2   
    #  -1 adjust for +;  -1 adjust for starting at 0 instead of 1
    
    
    hrsPerPeriod <- (periodPt/CatBins1$binsPerDay)*(modulo/60)
    if (Avg)  {    
      title(paste('CrossCorrelation\nFile 1 & 2, Avg of',CatBins1$animals,'Columns: ',nameVector1[1],' - ',nameVector1[endXrel],";",nameVector2[1],' - ',nameVector2[endXrel],'\n (File 2)',TimeBins2[startData2],'--',TimeBins2[CatBins2$endData],'\nvs (File1)',TimeBins1[startData1],'--',TimeBins1[CatBins1$endData]),cex.main=.9)
      #fileoutXcorrX <- sub(pattern=".", replacement="-Avg.", x=fileoutXcorr, fixed = TRUE)   # name of file for exported data
      #fileoutXcorrP <- sub(pattern=".", replacement="-Avg.", x=fileoutXcorr, fixed = TRUE)   # name of file for exported data
      #fileoutXcorrX <- sub(pattern=".", replacement="-Avg.", x=fileoutXcorr, fixed = TRUE)
      fileoutXcorrP <- sub(pattern="-fXCorr", replacement=paste("-Avg-fPgram.",sep=""), x=fileoutXcorr,fixed = TRUE)
    } else {
      title(paste('CrossCorrelation\nFile 1 & 2, Column',curX,': ',nameVector1[cnt],";",nameVector2[cnt],'\n(File 2)',TimeBins2[startData2],'--',TimeBins2[CatBins2$endData],'\nvs (File 1)',TimeBins1[startData1],'--',TimeBins1[CatBins1$endData]),cex.main=.9)
      #fileoutXcorrX <- sub(pattern=".", replacement=paste("-col",cnt,".",sep=""), x=fileoutXcorr,fixed = TRUE)
      #fileoutXcorrX <- sub(pattern=".", replacement=paste("-col",cnt,".",sep=""), x=fileoutXcorr,fixed = TRUE)
      fileoutXcorrP <- sub(pattern="-fXCorr", replacement=paste("-col",cnt,"-fPgram.",sep=""), x=fileoutXcorr,fixed = TRUE)
    }  # add file name, add info on 2nd data set?
    title(sub = paste("Lag (Count of ",CatBins1$newMinPerBin,"-min Bins)",sep=""),line=2)
    if (Rverbose!=-1){
      mtext(paste("Max Lag = ",MaxLag,'; Phase offset:',getCCF$lag[periodPt],' bins, ',getCCF$lag[periodPt]*CatBins1$newMinPerBin/60,' hr;  95% CI'),cex=.7,side=1,line=2.9)
    }
    dataVar<-var(DataList2[cnt,])*2     #  /n   7125.35  total power, then divide by n/w to get avg noise level
    # to test at 5% you have to take 2*(average variance-- final noise)
    noise<-dataVar/(length(DataList2[cnt,])/2)
    # Use the periodogram -- second data file data set only;  write result to file fileoutXcorrP
    getACV <- acf(DataList2[cnt,],type="covariance",plot=FALSE)
    CatPeriodogram<- CatPeri(getACV$acf,binsPerHr=CatBins2$binsPerDay/(modulo/60), minsPerBin=CatBins2$newMinPerBin, export=export, noise=noise*2,fileoutXcorrP, Hx=.2, cex=.75, Rverbose=Rverbose)    # get Units working
    # to restore to ACF, comment out above 2 lines, uncomment CatPeri below;  switch title used below
    #CatPeriodogram<- CatPeri(DataList2[cnt,],binsPerHr=CatBins2$binsPerDay/(modulo/60), minsPerBin=CatBins2$newMinPerBin, export=export, noise=noise, fileoutP=fileoutXcorrP,Hx=.2, cex=.75, Rverbose=Rverbose)    # get Units working

    #title('\n\nPeriodogram of File 2 data',cex.main=.9)
    title('\n\nSpectral plot -- FFT of autocovariance on File 2',cex.main=.9)
    n<-length(DataList2[cnt,])
    nfft<-(n/2)+1
    
    #------------------------
    
    #mtext(paste("Max Lag= ",MaxLag,";    1st Peak:",periodPt,'bins (', hrsPerPeriod,"hrs)"),cex=.8,side=1,line=2.9)
    if (Rverbose!=-1){
      mtext(paste('File 2: Total bins: ',CatBins2$dataPts/CatBins2$binPts,';     pts/bin: ',CatBins2$binPts,'(',CatBins2$newMinPerBin,'mins)'),side=1,line = 2.9, cex =.7)    #     bins/day: ',CatBins$binsPerDay
    }

    #mtext(thisTime,side=1,line = 5, cex =.8, adj=.8)
    mtext(paste("Span:",Span," Inc:",Increment),side=1,line = 3.75, cex =.5, adj=.1)
    mtext(paste(fileName,"   ",thisTime),side=1,line = 3.75, cex =.5, adj=.9)
    if (export==T){
      if (cnt==1){
        fileMatrix<-getCCF$lag
      }
      fileMatrix<-cbind(fileMatrix,getCCF$acf)
      #write.matrix(cbind(getCCF$lag,getCCF$acf), file = fileoutXcorrX, sep = "\t")   #  file 2 is the experimental data
    }
  }  # end for (cnt in 1:CatBins1$plotCnt)

  if (export==T){
    colnames(fileMatrix)<-NULL
    nameVector<-outer(FUN=paste,nameVector1,"x",nameVector2)
    write.matrix(rbind(cbind("XcorrLag",t(nameVector)),fileMatrix), file = fileoutXcorr, sep = "\t")
  }
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
