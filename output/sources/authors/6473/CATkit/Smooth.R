Smooth <-
function (endX,Avg=1, lum=4, k=6,yLab,modulo=1440, Rverbose=0, CatBins,fileName,fileNum,startData,export=F,fileoutSmooth="") {
#  starting column, ending column, Average or Individual, light level, #minutes between points, # points to bin, # minutes/unit (usually day)
# does a moving average across k elements, so K should be odd.  if it is even, more of the filter is forward in time than backward
  # http://www.cookbook-r.com/Manipulating_data/Calculating_a_moving_average/
  #require(delftfews)
  #library(stats)
#modulo <-1440
  #detach("signal", unload=TRUE)    #  fails:signal overrides the "filter" function from R base; causing this to fail
  #unloadNamespace("signal")   #  Error: package 'signal' is required by 'CATkit' so will not be detached
  #bins ##CatBins<-CatBin(TimeCol=TimeCol,startX, endX, Avg, lum, sizePts, binPts,modulo, startData=1,myData)
  # returns newMinPerBin,dataPts,binsPerDay,animals, startData, endData, DataList,plotCnt
  
#   ***********another way to do a rolling mean
#   rollmean <- function(x, n) {
#     out <- rep(NA, length(x))
#     
#     offset <- trunc(n / 2)
#     for (i in (offset + 1):(length(x) - n + offset + 1)) {
#       out[i] <- mean(x[(i - offset):(i + offset - 1)])
#     }
#     out
#   }
#   x <- seq(1, 3, length = 1e2) + runif(1e2)
#   plot(x)
#   lines(rollmean(x, 5), col = "blue", lwd = 2)
#   lines(rollmean(x, 10), col = "red", lwd = 2)
#   ********************************************

  startX<-2    #  time is in column 1
  DataList<-CatBins$DataList
  TimeBins<-CatBins$TimeBins
  #names(DataList)<-CatBins$binNames
  #myData<-cbind(CatBins$TimeBins,t(CatBins$DataList))
  op<-par()
  thisTime <- format(Sys.time(), "%b %d, %Y - %H:%M:%S");
  if (Avg){
    fileoutSmoothX <- sub(pattern=".", replacement="-Avg.", x=fileoutSmooth, fixed = TRUE)
  }    # otherwise use as given
#nameVector = paste(" ",names(DataList[,1:endX]),sep=" ")    #  do not include last column of lum
nameVector = CatBins$binNames[-1]     # exclude the DateTime header name
endXrel<-endX-startX+1
  par(new=FALSE)
for (cnt in 1:CatBins$plotCnt) {      #  1 plot per animal, for all animals 
  #par(mfrow=c(2,1))    #,omi = c(0,0,1,0)) 
  curX<-cnt+startX-1
  par(oma=c(2,0,0,0))
  if (is.na(yLab)){yAxisLab<-nameVector[cnt]} else {yAxisLab==yLab}
  # found at http://zoonek2.free.fr/UNIX/48_R/15_Time_Series.txt for smoothing noisy sunspot data
  plotPch<-20
  if (CatBins$endData>5000){
    plotPch<-"."    #  this is smaller, when there are many data
  }
  plot(DataList[cnt,], 
       type = 'p', pch=plotPch,
       ylab = yAxisLab, 
       xlab = paste("Time (#",CatBins$newMinPerBin,"-min Bins from ",TimeBins[1],")",sep=""),
       xaxt="n",
       main = "", col="mediumslateblue")
#  filter smooths taking values before and after   (doesn't work if lib{signal} is loaded!!!)
# get(x = "filter", pos = "package:stats")
  smoothData<-stats::filter(DataList[cnt,], rep(1/k,k))
  lines( smoothData,     #  same with method="convolution"),
         col = 'red', 
         lwd = 1)   #  ,names.arg = substr(TimeBins[],12,16) ,axis.lty = 1

  #text(0,0, labels = "Dark Onset-->                                                           ", srt = 90, pos = 1, cex=.5,xpd = TRUE)
  endList<-length(DataList[cnt,])
  dayLines = seq(0,endList,CatBins$binsPerDay)
  axis(1,at=dayLines,las=2, cex = .2)
  abline(v=dayLines,col="gray")

  
    if (Avg)  {    
        title(paste('Smoothing (k=',k,')\nFile',fileNum,', Avg across',CatBins$animals,'data columns:  ',nameVector[1],' - ',nameVector[endXrel],'\n',TimeBins[1],'   to   ',TimeBins[CatBins$endData]),cex.main=.9,col="blue")    #,xlab = paste("Bin count:   ",CatBins$binsPerDay,'bins per day'))
        #fileoutSmoothX <- sub(pattern=".", replacement="-Avg.", x=fileoutSmooth, fixed = TRUE)
    } else {
        title(paste('Smoothing (k=',k,')\nFile',fileNum,', Column ',curX,': ',nameVector[cnt],'\n',TimeBins[1],'   to   ',TimeBins[CatBins$endData]),cex.main=.9,col="blue")     #,xlab = paste("Bin count:   ",CatBins$binsPerDay,'bins per day'))
        #fileoutSmoothX <- sub(pattern=".", replacement=paste("-col",curX,".",sep=""), x=fileoutSmooth,fixed = TRUE)
    }  # add file name, change 5 to variable    tail(TimeBins[],n=1))
if (Rverbose!=-1){
  title(sub=paste(CatBins$sizePts,'mins/point;   ',CatBins$binPts,'points/bin;   ',CatBins$newMinPerBin,'mins/bin;     ',CatBins$binsPerDay,'bins/day'),cex.sub=.7,col="blue")       #  ";     \nBin count: ",CatBins$binsPerDay,'bins per day'),
}
  #mtext(paste("Dark Onset at bin",startData,"                  ",fileName,"   ",thisTime),side=1,line = 5, cex =.5, adj=.8)
  if (is.na(lum)){
    mtext("Starting with first data point.",side=1,line = 5, cex =.5, adj=.01)
  } else {
    mtext(paste("Dark Onset at bin",startData),side=1,line = 5, cex =.5, adj=.01)    # use index as bin#
  }
#mtext(paste("Dark Onset at bin",startData),side=1,line = 5, cex =.5, adj=.01)    # use index as bin#
  mtext(paste(fileName,"   ",thisTime),side=1,line = 5, cex =.5, adj=.8) 

if (export==T){
  if (cnt==1){
    fileMatrix<-TimeBins
  } 
  fileMatrix<-cbind(fileMatrix,smoothData)
  #write.matrix(cbind(getCCF$lag,getCCF$acf), file = fileoutXcorrX, sep = "\t")   #  file 2 is the experimental data
  }
}  # end for (cnt in 1:CatBins1$plotCnt)

if (export==T){
  colnames(fileMatrix)<-NULL
  write.matrix(rbind(cbind("DateTime",t(nameVector)),fileMatrix), file = fileoutSmooth, sep = "\t")
}
  #remove read only list components 
  op$cin<-NULL
  op$cra<-NULL
  op$csi<-NULL
  op$cxy<-NULL
  op$din<-NULL
  op$page<-NULL
  par(op)
#library(signal)
loadedNamespaces()
end
}
