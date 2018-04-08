CatWindow <-
function (window="Hanning", myData, Time, alpha=.54, dataN, Start, debug) {
    #  filter, Ycolumn, time column, alpha in window formula, count of data hours, where to start
    #  normal functions woudl use dataN as count of data, but this one is adapted for non-equidistant data, so it must be in terms of the data
    #  dataN must be N+dt hours of the data.
    #  do you need to add one for the ticks instead of the intervals  todo
    #  use midpoints???   todo
    #  how many time points?  average between time points?
    #  pass only the column and span being processed.  
    #  Time is the Time column values of the data being processed (from which the function subtracts the Starting time, to normalize). 
    #  dataN is the count of hours (because this is equidistant data, and N is time)
    #  Start is the starting time that needs to be subtracted from each span, so they are all relative
    Wsignal <- vector(length=dataN,mode="numeric")
    beta<-1-alpha

    #RefTime<-as.POSIXct(strptime(Time[1], "%Y%m%d%H%M"))
    #Ntime<-as.numeric(Time) - as.numeric(RefTime)
    #Htime<-Ntime/3600
    #date <- format(date, format = "%Y%m%d%H%M", tz = "", usetz = FALSE)
    #browser()
    #RefTime<-Time[1]
    #Time<-Time-RefTime
    Time<-Time-Start
    newData<-myData-mean(myData)    #  remove mean  -- it is added back in later

    if (window=='Hamming'){
      H<-alpha-beta*cos((2*pi*Time)/(dataN-1))
    } else {if (window=='Hanning'){
      H<-.5-.5*cos((2*pi*Time)/(dataN-1))   #  removed -1 9/18/13
    } else {if (window=='Bartlett'){
      H<-1-abs((Time-((dataN-1)/2))/((dataN-1)/2)) 
    } else {if (window=='Blackman'){
      a<-.16
      a0<-(1-a)/2
      a1<-1/2
      a2<-a/2
      H<-a0-a1*cos((2*pi*Time)/(dataN-1))+a2*cos((4*pi*Time)/(dataN-1))
    } 
    }}}
    if (window=='noTaper'){
      Wsignal<-myData
      H=Time
    } else {
      Wsignal<-newData*H+mean(myData)
    }
    
    #  need data to be multiplied by a numeric of time  -- hour count  -- is this continuous in # hrs?
    DataList <- data.frame(Time,Wsignal)
    str(DataList)
    #write.matrix(dfData, file = "~/R_WD/testDataFile.txt", sep = ",")
    
    #--opar = par() 
    if (window!='noFilt' && debug){
      op<-par()
      layout(matrix(c(1,2,3), 3, 1, byrow = TRUE), 
             widths=c(1), heights=c(1,1,1))
      par(mar=c(2,4,1,1), oma=c(1,1,1,1))
      plot(Time,myData, ylab="data", type="l",bty='n')#remove read only list components 
      plot(Time,H,ylab="window", type="l")      #  ,bty='n' 
      #plot(0:10, 0:10, type="n",xlab="",ylab="",axes=FALSE)
      plot(Time,Wsignal,ylab="data*window", type="l",bty='n')   #  axes=FALSE,
      op$cin<-NULL
      op$cra<-NULL
      op$csi<-NULL
      op$cxy<-NULL
      op$din<-NULL
      op$page<-NULL
      par(op)
      }
    return(list(data=Wsignal))
    
    end
  }
