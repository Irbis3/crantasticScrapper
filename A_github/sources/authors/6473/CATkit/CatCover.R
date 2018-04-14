CatCover <-
function (TimeCol, TimeFormat, valCols, sumCols, lum, export, percentMissing, maxGap, sizePts, binPts, Avg, Span, Increment, k, yLab, modulo,Rverbose, RmaxGap, tz,Skip,header,nFiles, Smoothing, Actogram,AutoCorr,CrossCorr,Multitaper,Console,Graphics, Darkness1,Darkness2,LagPcnt,File1,File2,File1out,File2out) {
  
#library("png")
#library("grid")

op <- par(no.readonly = TRUE) # the whole list of settable par's.
par(new=FALSE)
par(mar = c(0,1,0,1),oma=c(0,1,0,1))
#par(bg = "lightcoral")       #salmon
#layout(matrix(c(1,1,1,2,3,4,5,5,5), 3, 3, byrow = TRUE))
#layout(matrix(c(1,1,1,2,3,4,5,5,5,6,6,6), 4, 3, byrow = TRUE))
#--------------------------------------------

# read a sample file (R logo)
#if (.Platform$file.sep=="/"){
#  img <- readPNG("~/Documents/ChronobiologyLab/Routput/ParameterTemplate.png")   #   mac
#} else {
  pngLocation<-system.file(package="CATkit")
  img <- readPNG(paste(pngLocation,"/pngFiles/ParameterTemplate.png",sep=""))   #  not yet working on PC
#}

# if your R supports it, we'll plot it
if (exists("rasterImage")) { # can plot only in R 2.11.0 and higher
  xRange=2
  xStart=1
  #plot(xStart:xRange, type='n', xlab = "", ylab = "", axes=FALSE)
  

  #layout.show(6)    #  this does a new plot, seems like
  plot(xStart:xRange, type = "n", xlab = "An Example of This Font", ylab = "", font=1,  axes=FALSE,fg="darkslategray3",col.axis="cornflowerblue", bg="orange",col="gray", col.lab="cornsilk", col.main="darkmagenta", col.sub="brown1")
  #  fg colors the box
  
  #box("figure",lty="dashed", col="blue")
  #box("inner", lty="dotted", col="green")
  #box("outer", lty="solid", col="red")
  
  yrange <- range(img) 
  if (names(dev.cur()) == "windows") {
    # windows device doesn't support semi-transparency so we'll need
    # to flatten the image
    transparent <- img[,,4] == 0
    img <- as.raster(img[,,1:3])
    img[transparent] <- NA
    
    # interpolate must be FALSE on Windows, otherwise R will
    # try to interpolate transparency and fail
    #grid.raster(img, xStart,xStart, ,xRange,xRange, interpolate=FALSE)
    rasterImage(img, xStart,xStart,xRange,xRange, interpolate=FALSE)
    
  } else {
    # any reasonable device will be fine using alpha
    #grid.raster(img, xStart,xStart,xRange,xRange)
    rasterImage(img, xStart,xStart,xRange,xRange)
    
  }
}
valColsStr<-deparse(valCols)
sumColsStr<-deparse(sumCols)
TimeColStr<-deparse(TimeCol)
text(c(1.35),c(1.86),labels=c(format(Sys.time(), "%d %b, %Y--%H:%M:%S")),col="gray31", cex=.8, adj = .4)   #  adj = c(0,0) left justifies
text(c(1.22),c(1.31),labels=paste("Smoothing=",Smoothing, "\n(k=",k,")\n\nActogram=",Actogram,"\n(modulo=",modulo,")\n\nAutoCorr=",AutoCorr,"\n(LagPcnt=",LagPcnt,")\n\nCrossCorr=",CrossCorr,"\n\nyLab=","\"",yLab,"\""),col="gray31", cex=.6)   #  adj = c(0,0) left justifies
text(c(1.5),c(1.33),labels=paste("TimeFormat=",TimeFormat,"\nTimeCol=",TimeColStr, "\nvalCols=",valColsStr, ", sumCols=",sumColsStr,"\nAvg=",Avg,"\nsizePts=",sizePts, ", binPts=",binPts, "\nRmaxGap=",RmaxGap, "\nSpan =",Span, ", Increment=",Increment,"\nlum=",lum, "\nDarkness1=",Darkness1,"\nDarkness2=",Darkness2,"\nheader=",header,", Skip=",Skip, "\nnFiles=",nFiles),col="gray31", cex=.6)   #  adj = c(0,0) left justifies
text(c(1.78),c(1.37),labels=paste("Rverbose=",Rverbose, "\nConsole=",Console,"\nGraphics=",Graphics, "\nexport=",export ),col="gray31", cex=.6)   #  adj = c(0,0) left justifies
text(c(1.04),c(1.07),labels=paste("Input\nFile1 =",File1, "\nFile2 =",File2),col="white", cex=.6, adj=0)   #  adj = c(0,0) left justifies
if (!is.na(percentMissing[2])){
  text(c(1.4),c(1.11),labels=paste("File1: ",format(percentMissing[1],digits=4), "% missing data; max gap:",maxGap[1],";   File2: ",format(percentMissing[2],digits=4), "% missing data; max gap found:",maxGap[2],""),col="white", cex=.6, adj=0)   #  adj = c(0,0) left justifies
} else {
  text(c(1.5),c(1.11),labels=paste("File1: ",format(percentMissing[1],digits=4), "% missing data; max gap:",maxGap[1]),col="white", cex=.6, adj=0)   #  adj = c(0,0) left justifies
}
if (File1out!="" || File2out!=""){
  fnLen1<-nchar(File1out)
  fnStart1<-fnLen1-125
  if (fnStart1<1) {fnStart1<-1}
  fnLen2<-nchar(File2out)
  fnStart2<-fnLen2-125
  if (fnStart2<1) {fnStart2<-1}
  text(c(1.02),c(1.03),labels=paste("Output: Binned and interpolated data\nFile1out =...",substr(File1out,fnStart1,fnLen1), "\nFile2out =...",substr(File2out,fnStart2,fnLen2)),col="white", cex=.6, adj=0)   #  adj = c(0,0) left justifies
}
par(op)
}
