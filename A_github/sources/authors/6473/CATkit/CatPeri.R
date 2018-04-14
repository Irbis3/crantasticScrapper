CatPeri <-
function (MyData,binsPerHr, minsPerBin=1, export=FALSE, noise, fileoutP, Hx, cex, Rverbose=0,Debug=FALSE){
    #  http://stats.stackexchange.com/questions/12164/testing-significance-of-peaks-in-spectral-density
    #  http://cran.r-project.org/web/packages/GeneCycle/GeneCycle.pdf     see Fisher.g.test and periodogram and robust.g.test
    # http://www.biomedcentral.com/1471-2105/6/117   fisher.g.test
    # http://adsabs.harvard.edu/full/1990ApJ...348..700K
    #myData is a vector, not a matrix
    # dt in units of hours
    #source('~/Documents/Cathy/Neuroscience papers/capstone/Chronobiology/CosinorR/CosinorEQ/periCosinor.R') 
    # Use the periodogram
    if (.Platform$file.sep=="/"){
      m=regexec("[^/]*$",fileoutP)            #     mac=[^/]*$
    } else {
      m=regexec("[^\\]*$",fileoutP)           #./(.$)    pc=[^\\]*$
    }
    fileName1<-regmatches(fileoutP,m)
    peri=periCosinor(MyData,plot=FALSE)
    
    n<-length(MyData)
    nfft<-(n/2)          #  do not add one because it needs to be one less.
    
    #------------------------
    periC<-peri$c[2:nfft]
    periX<-1/periC
    periLastX<-1/peri$c[nfft]
    periY<-peri$peri[2:nfft]
    periA<-peri$amp[2:nfft]
    maxLineN<-which.max(periY)
    maxLine<-max(periY)
    #yLim<-c(NULL,NULL)
    if (noise>maxLine){
      plot(periX,periY,ylab='Power',xlab='',ylim=c(0,noise) ,type='h') 
      maxLine<-noise
    } else {
      plot(periX,periY,ylab='Power',xlab='', type='h') 
    }
    xLast<-length(periX)
    #title(sub = paste("Frequency (in #",minsPerBin,"-min bins/cycle)", sep=""),line=2)
    title(sub = paste("Frequency (Cycles/Total Bins)", sep=""),line=2)
    abline(h=noise,col="blue",lty="dotted")
    text(periX[xLast],noise,paste("noise=\n",format(noise,digits=5)), pos=2, cex=cex-.2)
    
    h<-maxLine*.8
    lf<-h*Hx*.5       # used to be .7   (minimum .4)
    s<-periLastX
    text(.5*s,h+(2*lf),"Spectral lines of four largest amplitudes",cex.main=.9)
    if (export==T){
      text(.5*s,h+(1.2*lf),paste("See data file",fileName1), cex=cex-.1)
    }
    if (Rverbose==0){
      text(c(.41*s,.61*s),c(h,h,h,h,h),labels=c("Period in #hrs","Period in #Bins"),adj = c(0,0), cex=cex)
      h<-h-(lf)
      text(c(.42*s,.62*s),c(h,h,h,h,h),labels=c("---------","----------"),adj = c(0,0), cex=cex)
    } else  if (Rverbose>0){
      text(c(.11*s,.21*s,.41*s,.61*s,.81*s),c(h,h,h,h,h),labels=c("Line #","Frequency","Period in #hrs","Period in #Bins","Amplitude / Power"),adj = c(0,0), cex=cex)
      h<-h-(lf)
      text(c(.11*s,.21*s,.42*s,.62*s,.81*s),c(h,h,h,h,h),labels=c("----","--------","---------","----------"," ----------"),adj = c(0,0), cex=cex)
    }  # Rverbose=-1 will print nothing on the graph
    
    if (export==T){
      write.matrix(rbind(cbind("peri","freq","cycle","amp","phase"),cbind(peri$peri,peri$f,peri$c,peri$amp,peri$phase)), file = fileoutP, sep = "\t")
    }
    
    binPeaks<-rev(sort(periY))
    maxLineN1<-which(periY==binPeaks[1])
    maxLineN2<-which(periY==binPeaks[2])
    maxLineN3<-which(periY==binPeaks[3])
    maxLineN4<-which(periY==binPeaks[4])

    if (Rverbose==0){
      h<-h-lf
      text(c(.43*s,.63*s),c(h,h,h,h,h),labels=c(format(periC[maxLineN1]/binsPerHr,digits=7),format(periC[maxLineN1],digits=7)),adj = c(0,0), cex=cex)
      h<-h-lf       
      text(c(.43*s,.63*s),c(h,h,h,h,h),labels=c(format(periC[maxLineN2]/binsPerHr,digits=7),format(periC[maxLineN2],digits=7)),adj = c(0,0), cex=cex)
      h<-h-lf
      text(c(.43*s,.63*s),c(h,h,h,h,h),labels=c(format(periC[maxLineN3]/binsPerHr,digits=7),format(periC[maxLineN3],digits=7)),adj = c(0,0), cex=cex)
      h<-h-lf
      text(c(.43*s,.63*s),c(h,h,h,h,h),labels=c(format(periC[maxLineN4]/binsPerHr,digits=7),format(periC[maxLineN4],digits=7)),adj = c(0,0), cex=cex)
    } else if (Rverbose>0){
      h<-h-lf
      text(c(.12*s,.21*s,.43*s,.63*s,.8*s),c(h,h,h,h,h),labels=c(maxLineN1,format(1/periC[maxLineN1],digits=7),format(periC[maxLineN1]/binsPerHr,digits=7),format(periC[maxLineN1],digits=7),paste(format(periA[maxLineN1],digits=4),"/",format(periY[maxLineN1],digits=7))),adj = c(0,0), cex=cex)
      h<-h-lf       
      text(c(.12*s,.21*s,.43*s,.63*s,.8*s),c(h,h,h,h,h),labels=c(maxLineN2,format(1/periC[maxLineN2],digits=7),format(periC[maxLineN2]/binsPerHr,digits=7),format(periC[maxLineN2],digits=7),paste(format(periA[maxLineN2],digits=4),"/",format(periY[maxLineN2],digits=7))),adj = c(0,0), cex=cex)
      h<-h-lf
      text(c(.12*s,.21*s,.43*s,.63*s,.8*s),c(h,h,h,h,h),labels=c(maxLineN3,format(1/periC[maxLineN3],digits=7),format(periC[maxLineN3]/binsPerHr,digits=7),format(periC[maxLineN3],digits=7),paste(format(periA[maxLineN3],digits=4),"/",format(periY[maxLineN3],digits=7))),adj = c(0,0), cex=cex)
      h<-h-lf
      text(c(.12*s,.21*s,.43*s,.63*s,.8*s),c(h,h,h,h,h),labels=c(maxLineN4,format(1/periC[maxLineN4],digits=7),format(periC[maxLineN4]/binsPerHr,digits=7),format(periC[maxLineN4],digits=7),paste(format(periA[maxLineN4],digits=4),"/",format(periY[maxLineN4],digits=7))),adj = c(0,0), cex=cex)
    }   # Rverbose=-1 will print nothing on the graph
    #-------------------
    # peri=peri model,f=freq,c=cycles,amp=amplitude,phase=phase
    if (Rverbose>1){
      print(peri$amp)
      print(peri$f)
      print(peri$phase)
      print(peri$c)
      print(peri$realpart)        # ~B?
      print(peri$imagpart)        # ~G?
      new<-(peri$phase/pi)*180}
  }
