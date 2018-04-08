AutoCorr <-
function (endX, Avg=1, lum=4, modulo=1440, Rverbose=0, CatBins, fileName,fileNum, Span, Increment,xStartSpans=1,xEndSpan,LagPcnt=1/3,export=F,fileoutCorr="") {
#  starting column, ending column, Average or Individual, #minutes between points, # points to bin, # minutes/unit (usually day)
# Venables & Ripley Modern Applied Statistics on S: Note that approximate 95% confidence limits are shown for the autocorrelation plots; 
  #these are for an independent series for which Pt = I(t = 0). As with a time series a priori one is expecting autocorrelation, these 
  #limits must be viewed with caution. In particular, if any Pt is non-zero, all the limits are invalid.
  #require(delftfews)
  #source('~/Documents/Cathy/Neuroscience papers/capstone/Bartolomucci/chrono data/R-functions/CatPeri/CatPeri.R')
  #modulo <- 1440
  startData <-1
  #@bins CatBins<-CatBin(TimeCol=TimeCol,startX, endX, Avg, lum, sizePts, binPts,modulo, startData=1, myData)   #  dark onset is not used
  # returns newMinPerBin,dataPts,binsPerDay,dayPts,dayCnt,animals, endData, DataList,plotCnt
  
  DataList<-CatBins$DataList
  TimeBins<-CatBins$TimeBins
  startX<-2    #  time is in column 1
  startDark <-as.character(format(as.POSIXct(TimeBins[startData], origin="1970-01-01",tz="GMT"),"%H:%M:%S"))    #  get index of first darkness value
  op<-par()
  thisTime <- format(Sys.time(), "%b %d, %Y - %H:%M:%S");
  nameVector = CatBins$binNames[-1]     # exclude the DateTime header name
  endXrel<-endX-startX+1
  #nameVector <- paste(" ",names(myData[,startX:endX]),sep=" ")
  if (Avg){
    fileoutCorr <- sub(pattern=".", replacement="-Avg.", x=fileoutCorr, fixed = TRUE)
  } #  otherwise use filename as passed

  #par(mfrow=c(2,1), oma = c(2,0,0,0),mar=c(2,4,4,2))   #,omi = c(0,0,1,0)) old
  layout(matrix(c(1,2), 2, 1, byrow = TRUE), 
         widths=c(1), heights=c(1,1))
  par( mar=c(4,4,4,1),oma = c(2,0,0,0))
  #  par(mfrow=c(2,1), mar = c(3, 4, 3, 0),oma=c(2,0,0,1))  cc
  endXrel<-endX-startX+1
for (cnt in 1:CatBins$plotCnt) {      #  1 plot per animal, for all animals 
  curX<-cnt+startX-1
  #MaxLag <- ceiling((CatBins$dataPts/CatBins$binPts)*LagPcnt)
  MaxLag <- round((CatBins$dataPts/CatBins$dayPts)*LagPcnt)*CatBins$dayPts        #dayPts is really pts in a modulus (chosen period)
  if (MaxLag<CatBins$dayPts){     #  cannot be less than one period
    MaxLag<-CatBins$dayPts
  }

	getACF <- acf(DataList[cnt,],main="",type="correlation",ylab="Correlation coefficient (r)",lag.max = MaxLag,xlab = "",panel.first = abline(v=seq(from=0, to=MaxLag, by=(CatBins$binsPerDay)),xaxt="n",col="darkslategray",lty="dotted"))
  #str(getACF)
  if (cnt==1){
    fileMatrix<-getACF$lag
    }
  #grid(nx=modulo/30,ny=NA)   # rep(x=(modulo/60),times=10)
  # Default is 10*log10(N/m) where N is the number of observations and m the number of series. 
  # Will be automatically limited to one less than the number of observations in the series.
  maxCheck <- CatBins$dayPts/2         #  get half again as many points as length of day
	periodPt <- which.max(getACF$acf[maxCheck:(CatBins$dayPts+maxCheck)]) + maxCheck - 2   
  #  -1 adjust for +;  -1 adjust for starting at 0 instead of 1
  hrsPerPeriod <- (periodPt/CatBins$binsPerDay)*(modulo/60)

    if (Avg)  {    # AutoCorrelation
        title(paste('AutoCorrelation\nFile',fileNum,', Avg across',CatBins$animals,'Columns: ',nameVector[1],' - ',nameVector[endXrel],'\n',TimeBins[startData],' -- ',TimeBins[CatBins$endData]),cex.main=.9)
        #fileoutCorr <- sub(pattern=".", replacement="-Avg.", x=fileoutCorr, fixed = TRUE)
        #fileoutCorrP <- sub(pattern=".", replacement="-Avg.", x=fileoutCorr, fixed = TRUE)
        fileoutCorrP <- sub(pattern="-fCorr", replacement=paste("-Avg-fPgram",sep=""), x=fileoutCorr,fixed = TRUE)
    } else {
        title(paste('AutoCorrelation\nFile',fileNum,', Column',curX,': ',nameVector[cnt],'\n',TimeBins[startData],' -- ',TimeBins[CatBins$endData]),cex.main=.9)
        #fileoutCorr <- sub(pattern=".", replacement=paste("-col",curX,".",sep=""), x=fileoutCorr,fixed = TRUE)
        #fileoutCorrP <- sub(pattern=".", replacement=paste("-col",curX,".",sep=""), x=fileoutCorr,fixed = TRUE)
        fileoutCorrP <- sub(pattern="-fCorr", replacement=paste("-col",curX,"-fPgram",sep=""), x=fileoutCorr,fixed = TRUE)
    }
  #title(xlab = paste("Max Lag= ",MaxLag,";    1st Peak:",periodPt,'bins (', hrsPerPeriod,"hrs)"),line=2)
  title(sub = paste("Lag (Number of ",CatBins$newMinPerBin,"-min Bins)",sep=""),line=2)
  if (Rverbose!=-1){
  #mtext(paste("Max Lag= ",MaxLag,";    1st Peak:",periodPt,'bins (', hrsPerPeriod,"hrs)"),cex=.7,side=1,line=2.9)
  mtext(paste("Max Lag= ",MaxLag,";    Grid lines every modulo=", modulo,"mins (",modulo/60,"hrs);  95% CI"),cex=.7,side=1,line=2.9)
  }
  # Use the periodogram

  #dataVar<-var(DataList[cnt,])*2     #  /n   7125.35  total power, then divide by n/w to get avg noise level
  #dataVarS<-sd(DataList[cnt,])^2/(length(DataList[cnt,])/2)
  # to test at 5% you have to take 2*(average variance-- final noise)
  #noise<-dataVar/(length(DataList[cnt,])/2)    # length in periodogram is 1/2 length of data
  noise<-var(DataList[cnt,])*2
  #cat("Y",nameVector[cnt],"; var",var(DataList[cnt,]),"sd2",dataVarS,"; length",length(DataList[cnt,])/2,"; noise at 10%", noise,"; noise at 5%",dataVar/(length(DataList[cnt,])/2)*2,"\n")
  cat("Y",nameVector[cnt],"; var",var(DataList[cnt,]),"; length",length(DataList[cnt,])/2,"; noise at 10%", noise,"; noise at 5%",noise*2,"\n")
  
  CatPeriodogram<- CatPeri(DataList[cnt,],binsPerHr=CatBins$binsPerDay/(modulo/60), minsPerBin=CatBins$newMinPerBin, export=export, noise=noise*2,fileoutCorrP, Hx=.2, cex=.75, Rverbose=Rverbose)    # get Units working

  #View(DataList[cnt,])
  title('\n\nPeriodogram by FFT',cex.main=.9)
  n<-length(DataList[cnt,])
  nfft<-(n/2)+1
  
  #------------------------
  
  #mtext(paste("Max Lag= ",MaxLag,";    1st Peak:",periodPt,'bins (', hrsPerPeriod,"hrs)"),cex=.8,side=1,line=2.9)
  if (Rverbose!=-1){
    mtext(paste('Total bins: ',CatBins$dataPts/CatBins$binPts,';     pts/bin: ',CatBins$binPts,'(',CatBins$newMinPerBin,'mins)'),side=1,line = 2.9, cex =.7)    #     bins/day: ',CatBins$binsPerDay
  }
  mtext(paste("Span:",Span," Inc:",Increment),side=1,line = 4, cex =.5, adj=0)
  mtext(paste(fileName,"   ",thisTime),side=1,line = 4, cex =.5, adj=.9)
  ###print(summary(getACFvar))
  if (export==T){
    #print(fileoutCorr)
    #peri=peri model,f=freq,c=cycles,amp=amplitude,phase=phase
    fileMatrix<-cbind(fileMatrix,getACF$acf)
      }
  }  # end for (cnt in 1:CatBins$plotCnt)

  if (export==T){
    colnames(fileMatrix)<-NULL
  write.matrix(rbind(cbind("lag",t(nameVector)),fileMatrix), file = fileoutCorr, sep = "\t")
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
